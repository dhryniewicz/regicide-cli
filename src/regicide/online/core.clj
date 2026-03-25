(ns regicide.online.core
  "Entry point for Regicide online multiplayer mode.
   Run with: clj -M:online"
  (:require [regicide.online.client :as client]
            [regicide.online.auth :as auth]
            [regicide.online.lobby :as lobby]
            [regicide.online.game-loop :as game-loop]
            [regicide.cli.terminal :as term]))

;; ---------------------------------------------------------------------------
;; Session management
;; ---------------------------------------------------------------------------

(defn- get-or-create-uid!
  "Get existing UID from saved credentials, or authenticate fresh."
  []
  (if-let [creds (auth/load-credentials)]
    (:uid creds)
    (:uid (client/authenticate!))))

;; ---------------------------------------------------------------------------
;; Main flow
;; ---------------------------------------------------------------------------

(defn- run-join-game! [uid]
  (let [player-name (lobby/prompt-player-name!)
        lobby-code  (lobby/prompt-lobby-code!)]
    (when (seq lobby-code)
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
  (client/init!)
  (let [uid (get-or-create-uid!)]
    (println (str "Authenticated as: " uid))
    (term/enable-raw-mode!)
    (try
      (main-menu-loop! uid)
      (print term/clear-screen)
      (println "\nThanks for playing!")
      (flush)
      (finally
        (term/disable-raw-mode!)))))
