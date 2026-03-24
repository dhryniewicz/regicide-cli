(ns regicide.online.core
  "Entry point for Regicide online multiplayer mode.
   Run with: clj -M:online"
  (:require [regicide.online.client :as client]
            [regicide.online.auth :as auth]
            [regicide.online.lobby :as lobby]
            [regicide.online.game-loop :as game-loop]
            [regicide.cli.terminal :as term]
            [regicide.cli.display :as display]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.io File]))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(def ^:private default-config-path
  (str (System/getProperty "user.home") "/.regicide/firebase-config.edn"))

(defn- load-config
  "Load Firebase config from ~/.regicide/firebase-config.edn or environment."
  []
  (let [config-file (File. ^String default-config-path)]
    (if (.exists config-file)
      (edn/read-string (slurp config-file))
      ;; Fall back to environment variables
      (let [db-url (System/getenv "FIREBASE_DATABASE_URL")
            sa-path (System/getenv "GOOGLE_APPLICATION_CREDENTIALS")]
        (when db-url
          {:database-url         db-url
           :service-account-path sa-path})))))

;; ---------------------------------------------------------------------------
;; Session management
;; ---------------------------------------------------------------------------

(defn- get-or-create-uid!
  "Get existing UID from saved credentials, or generate a new anonymous one."
  []
  (if-let [creds (auth/load-credentials)]
    (:uid creds)
    (let [uid (str "anon-" (subs (str (java.util.UUID/randomUUID)) 0 8))]
      (auth/save-credentials! {:uid uid})
      uid)))

;; ---------------------------------------------------------------------------
;; Main flow
;; ---------------------------------------------------------------------------

(defn- run-create-game! [uid]
  (let [player-name (lobby/prompt-player-name!)
        menu-choice (lobby/show-lobby-menu!)]
    (when (= :create (:action menu-choice))
      (let [num-players (:num-players menu-choice)
            lobby-id    (lobby/create-lobby! uid player-name num-players)]
        (println (str "\n  Lobby created! Code: " (subs lobby-id (max 0 (- (count lobby-id) 6)))))
        (println (str "  Full ID: " lobby-id))
        (println (str "  Waiting for " (dec num-players) " more player(s)..."))
        (flush)
        (when-let [game-id (lobby/wait-for-game-start! lobby-id)]
          (game-loop/run-game-loop! game-id uid))))))

(defn- run-join-game! [uid]
  (let [player-name (lobby/prompt-player-name!)
        lobby-code  (lobby/prompt-lobby-code!)]
    (when (seq lobby-code)
      ;; Try to find the lobby by code suffix
      (let [lobbies (client/read-once "lobbies")
            matching (when lobbies
                       (->> lobbies
                            (filter (fn [[k _]] (.endsWith (name k) lobby-code)))
                            first))]
        (if matching
          (let [lobby-id (name (key matching))]
            (lobby/join-lobby! lobby-id uid player-name)
            (println (str "\n  Joined lobby " lobby-code "!"))
            (flush)
            (when-let [game-id (lobby/wait-for-game-start! lobby-id)]
              (game-loop/run-game-loop! game-id uid)))
          (do (println (str "\n  Lobby '" lobby-code "' not found."))
              (println "  Press any key...")
              (flush)
              (term/read-key)))))))

(defn- main-menu-loop! [uid]
  (loop []
    (print term/clear-screen)
    (let [choice (lobby/show-lobby-menu!)]
      (case (:action choice)
        :create
        (let [num-players (:num-players choice)
              player-name (lobby/prompt-player-name!)
              lobby-id    (lobby/create-lobby! uid player-name num-players)]
          (println (str "\n  Lobby created! Share this code: "
                        (subs lobby-id (max 0 (- (count lobby-id) 6)))))
          (flush)
          (if-let [game-id (lobby/wait-for-game-start! lobby-id)]
            (game-loop/run-game-loop! game-id uid)
            (recur)))

        :join
        (do (run-join-game! uid)
            (recur))

        :back nil))))

(defn -main [& _args]
  (let [config (load-config)]
    (when-not config
      (println "Firebase not configured.")
      (println "Create ~/.regicide/firebase-config.edn with:")
      (println "  {:database-url \"https://your-project.firebaseio.com\"")
      (println "   :service-account-path \"/path/to/service-account.json\"}")
      (println)
      (println "Or set environment variables:")
      (println "  FIREBASE_DATABASE_URL=https://your-project.firebaseio.com")
      (println "  GOOGLE_APPLICATION_CREDENTIALS=/path/to/service-account.json")
      (System/exit 1))

    (client/init! config)
    (let [uid (get-or-create-uid!)]
      (println (str "Authenticated as: " uid))
      (term/enable-raw-mode!)
      (try
        (main-menu-loop! uid)
        (print term/clear-screen)
        (println "\nThanks for playing!")
        (flush)
        (finally
          (term/disable-raw-mode!))))))
