(ns regicide.online.client
  "Firebase Realtime Database client via REST API.
   No service account needed — uses the public database URL and anonymous auth.
   This is the client-side equivalent of the Firebase JS SDK."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [regicide.online.auth :as auth])
  (:import [java.net URI HttpURLConnection URL]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
                          HttpResponse HttpResponse$BodyHandlers]
           [java.util.concurrent CountDownLatch TimeUnit Executors
                                 ScheduledExecutorService]
           [java.io BufferedReader InputStreamReader]))

;; ---------------------------------------------------------------------------
;; Configuration (baked-in, not secret)
;; ---------------------------------------------------------------------------

(def ^:private config
  {:database-url "https://regicide-online-default-rtdb.europe-west1.firebasedatabase.app"
   :api-key      "AIzaSyBmkdYxvzthlEOS03yEaCzi3J_m6vIZ52I"})

(defonce ^:private http-client
  (delay (HttpClient/newHttpClient)))

(defonce ^:private auth-token (atom nil))

;; ---------------------------------------------------------------------------
;; Anonymous Auth via Firebase Auth REST API
;; ---------------------------------------------------------------------------

(defn authenticate!
  "Sign in anonymously via Firebase Auth REST API.
   Returns and caches {:uid, :idToken, :refreshToken}."
  []
  (if-let [cached @auth-token]
    cached
    (let [url (str "https://identitytoolkit.googleapis.com/v1/accounts:signUp?key="
                   (:api-key config))
          body (json/write-str {:returnSecureToken true})
          request (-> (HttpRequest/newBuilder)
                      (.uri (URI/create url))
                      (.header "Content-Type" "application/json")
                      (.POST (HttpRequest$BodyPublishers/ofString body))
                      (.build))
          response (.send @http-client request (HttpResponse$BodyHandlers/ofString))
          data (json/read-str (.body response) :key-fn keyword)]
      (if (:localId data)
        (let [creds {:uid          (:localId data)
                     :id-token     (:idToken data)
                     :refresh-token (:refreshToken data)}]
          (reset! auth-token creds)
          (auth/save-credentials! {:uid (:uid creds)})
          creds)
        (throw (ex-info "Firebase anonymous auth failed" {:response data}))))))

(defn- auth-param
  "Return the auth query parameter for authenticated RTDB requests."
  []
  (str "auth=" (:id-token @auth-token)))

;; ---------------------------------------------------------------------------
;; REST API helpers
;; ---------------------------------------------------------------------------

(defn- db-url
  "Build a full RTDB REST URL for the given path."
  ([path] (db-url path false))
  ([path with-auth?]
   (let [base (str (:database-url config) "/" path ".json")]
     (if with-auth?
       (str base "?" (auth-param))
       base))))

(defn- http-get [url]
  (let [request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.GET)
                    (.build))
        response (.send @http-client request (HttpResponse$BodyHandlers/ofString))
        status (.statusCode response)]
    (when (<= 200 status 299)
      (let [body (.body response)]
        (when (and body (not= body "null"))
          (json/read-str body :key-fn keyword))))))

(defn- http-put [url value]
  (let [body (json/write-str value)
        request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.header "Content-Type" "application/json")
                    (.PUT (HttpRequest$BodyPublishers/ofString body))
                    (.build))
        response (.send @http-client request (HttpResponse$BodyHandlers/ofString))]
    (json/read-str (.body response) :key-fn keyword)))

(defn- http-post [url value]
  (let [body (json/write-str value)
        request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.header "Content-Type" "application/json")
                    (.POST (HttpRequest$BodyPublishers/ofString body))
                    (.build))
        response (.send @http-client request (HttpResponse$BodyHandlers/ofString))]
    (json/read-str (.body response) :key-fn keyword)))

(defn- http-delete [url]
  (let [request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.DELETE)
                    (.build))]
    (.send @http-client request (HttpResponse$BodyHandlers/ofString))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn init!
  "Initialize the client (authenticates anonymously)."
  []
  (authenticate!))

(defn read-once
  "Read a value at the given path. Returns parsed JSON as Clojure data."
  [path]
  (http-get (db-url path true)))

(defn write!
  "Write a value to the given path (PUT)."
  [path value]
  (http-put (db-url path true) value))

(defn push!
  "Push a new child under the given path (POST). Returns the generated key."
  [path value]
  (let [result (http-post (db-url path true) value)]
    (:name result)))

;; ---------------------------------------------------------------------------
;; Game-specific helpers
;; ---------------------------------------------------------------------------

(defn send-action!
  "Send a player action to /games/{gameId}/actions."
  [game-id action-data]
  (push! (str "games/" game-id "/actions") action-data))

(defn read-public-state
  "Read the current public game state."
  [game-id]
  (read-once (str "games/" game-id "/public")))

(defn read-hand
  "Read the current player's hand."
  [game-id uid]
  (read-once (str "games/" game-id "/hands/" uid)))

(defn read-meta
  "Read game metadata."
  [game-id]
  (read-once (str "games/" game-id "/meta")))

(defn read-error
  "Read and clear the error for a specific uid."
  [game-id uid]
  (let [error-data (read-once (str "games/" game-id "/errors/" uid))]
    (when error-data
      (write! (str "games/" game-id "/errors/" uid) nil)
      (:message error-data))))

;; ---------------------------------------------------------------------------
;; SSE listener (Firebase RTDB supports Server-Sent Events)
;; ---------------------------------------------------------------------------

(defn listen!
  "Listen to a path via SSE. Calls (callback value) on each update.
   Returns a function that stops the listener."
  [path callback]
  (let [running (atom true)
        url (str (db-url path true))
        thread (Thread.
                 (fn []
                   (while @running
                     (try
                       (let [conn (doto (.openConnection (URL. url))
                                    (.setRequestProperty "Accept" "text/event-stream")
                                    (.setDoInput true)
                                    (.connect))
                             reader (BufferedReader.
                                      (InputStreamReader. (.getInputStream conn)))]
                         (loop []
                           (when @running
                             (when-let [line (.readLine reader)]
                               (when (str/starts-with? line "data: ")
                                 (let [data-str (subs line 6)]
                                   (when (and (not= data-str "null")
                                              (seq data-str))
                                     (try
                                       (let [event (json/read-str data-str :key-fn keyword)]
                                         (when-let [data (:data event)]
                                           (callback data)))
                                       (catch Exception _)))))
                               (recur)))))
                       (catch Exception _
                         (when @running
                           (Thread/sleep 2000)))))))]
    (.setDaemon thread true)
    (.start thread)
    (fn []
      (reset! running false))))
