(ns regicide.online.client
  "Firebase Realtime Database client wrapper for the CLI.
   Handles initialization, authentication, listening, and writing actions."
  (:require [regicide.online.auth :as auth])
  (:import [com.google.auth.oauth2 GoogleCredentials]
           [com.google.firebase FirebaseApp FirebaseOptions]
           [com.google.firebase.database
            FirebaseDatabase DatabaseReference ValueEventListener
            DataSnapshot ChildEventListener DatabaseError]
           [java.util.concurrent CountDownLatch TimeUnit]
           [java.util HashMap ArrayList]))

;; ---------------------------------------------------------------------------
;; Initialization
;; ---------------------------------------------------------------------------

(defonce ^:private firebase-app (atom nil))
(defonce ^:private db (atom nil))

(defn init!
  "Initialize the Firebase client with service account credentials.
   config should contain :database-url and optionally :service-account-path."
  [config]
  (when-not @firebase-app
    (let [options (-> (FirebaseOptions/builder)
                      (.setCredentials (if-let [sa-path (:service-account-path config)]
                                         (GoogleCredentials/fromStream
                                           (java.io.FileInputStream. ^String sa-path))
                                         (GoogleCredentials/getApplicationDefault)))
                      (.setDatabaseUrl (:database-url config))
                      (.build))
          app (FirebaseApp/initializeApp options)]
      (reset! firebase-app app)
      (reset! db (FirebaseDatabase/getInstance app)))))

(defn database
  "Get the Firebase database instance."
  []
  @db)

(defn ref
  "Get a DatabaseReference for the given path."
  [path]
  (.getReference ^FirebaseDatabase @db ^String path))

;; ---------------------------------------------------------------------------
;; Read operations
;; ---------------------------------------------------------------------------

(defn read-once
  "Read a value at the given path, synchronously. Returns the Java object value."
  [path]
  (let [result (atom nil)
        error  (atom nil)
        latch  (CountDownLatch. 1)]
    (.addListenerForSingleValueEvent
      (ref path)
      (reify ValueEventListener
        (onDataChange [_ snapshot]
          (reset! result (.getValue ^DataSnapshot snapshot))
          (.countDown latch))
        (onCancelled [_ db-error]
          (reset! error (.getMessage ^DatabaseError db-error))
          (.countDown latch))))
    (.await latch 10 TimeUnit/SECONDS)
    (when @error
      (throw (ex-info (str "Firebase read error: " @error) {:path path})))
    @result))

;; ---------------------------------------------------------------------------
;; Listeners
;; ---------------------------------------------------------------------------

(defn listen!
  "Attach a value listener to a path. Calls (callback value) on each update.
   Returns a function that removes the listener when called."
  [path callback]
  (let [db-ref (ref path)
        listener (reify ValueEventListener
                   (onDataChange [_ snapshot]
                     (callback (.getValue ^DataSnapshot snapshot)))
                   (onCancelled [_ db-error]
                     (println "Listener error:" (.getMessage ^DatabaseError db-error))))]
    (.addValueEventListener db-ref listener)
    (fn [] (.removeEventListener db-ref listener))))

;; ---------------------------------------------------------------------------
;; Write operations
;; ---------------------------------------------------------------------------

(defn write!
  "Write a value to the given path. Blocks until complete."
  [path value]
  (let [latch (CountDownLatch. 1)
        error (atom nil)]
    (.setValue (ref path) value
      (reify com.google.firebase.database.DatabaseReference$CompletionListener
        (onComplete [_ db-error _ref]
          (when db-error
            (reset! error (.getMessage ^DatabaseError db-error)))
          (.countDown latch))))
    (.await latch 10 TimeUnit/SECONDS)
    (when @error
      (throw (ex-info (str "Firebase write error: " @error) {:path path})))))

(defn push!
  "Push a new child under the given path. Returns the generated key."
  [path value]
  (let [new-ref (.push (ref path))
        key     (.getKey new-ref)
        latch   (CountDownLatch. 1)
        error   (atom nil)]
    (.setValue new-ref value
      (reify com.google.firebase.database.DatabaseReference$CompletionListener
        (onComplete [_ db-error _ref]
          (when db-error
            (reset! error (.getMessage ^DatabaseError db-error)))
          (.countDown latch))))
    (.await latch 10 TimeUnit/SECONDS)
    (when @error
      (throw (ex-info (str "Firebase push error: " @error) {:path path})))
    key))

;; ---------------------------------------------------------------------------
;; Game-specific helpers
;; ---------------------------------------------------------------------------

(defn send-action!
  "Send a player action to /games/{gameId}/actions.
   action-data should be a map with :type, :uid, :version, and any action-specific fields."
  [game-id action-data]
  (let [action-map (HashMap.)]
    (doseq [[k v] action-data]
      (.put action-map (name k) v))
    (push! (str "games/" game-id "/actions") action-map)))

(defn- java->clj
  "Convert Java Map/List/value to Clojure data structures."
  [obj]
  (cond
    (instance? java.util.Map obj)
    (into {} (map (fn [[k v]] [(keyword k) (java->clj v)]) obj))

    (instance? java.util.List obj)
    (mapv java->clj obj)

    :else obj))

(defn read-public-state
  "Read the current public game state, returning a Clojure map."
  [game-id]
  (java->clj (read-once (str "games/" game-id "/public"))))

(defn read-hand
  "Read the current player's hand, returning a vector of card maps."
  [game-id uid]
  (let [raw (read-once (str "games/" game-id "/hands/" uid))]
    (when raw
      (java->clj raw))))

(defn read-meta
  "Read game metadata."
  [game-id]
  (java->clj (read-once (str "games/" game-id "/meta"))))

(defn read-error
  "Read and clear the error for a specific uid."
  [game-id uid]
  (let [error-data (java->clj (read-once (str "games/" game-id "/errors/" uid)))]
    (when error-data
      ;; Clear the error after reading
      (write! (str "games/" game-id "/errors/" uid) nil)
      (:message error-data))))
