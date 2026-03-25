(ns regicide.online.lobby
  "Lobby management for CLI client. Create, join, list, and wait for games."
  (:require [regicide.online.client :as client]
            [regicide.cli.terminal :as term])
  (:import [java.util HashMap]
           [java.util.concurrent CountDownLatch TimeUnit]))

;; ---------------------------------------------------------------------------
;; Lobby operations
;; ---------------------------------------------------------------------------

(defn create-lobby!
  "Create a new lobby. Returns the lobby ID."
  [uid player-name num-players]
  (let [lobby (doto (HashMap.)
                (.put "host" uid)
                (.put "numPlayers" num-players)
                (.put "status" "waiting")
                (.put "createdAt" (System/currentTimeMillis))
                (.put "players" (doto (HashMap.)
                                  (.put uid (doto (HashMap.)
                                              (.put "name" player-name)
                                              (.put "ready" true))))))]
    (client/push! "lobbies" lobby)))

(defn join-lobby!
  "Join an existing lobby. Returns true on success."
  [lobby-id uid player-name]
  (let [player-data (doto (HashMap.)
                      (.put "name" player-name)
                      (.put "ready" true))]
    (client/write! (str "lobbies/" lobby-id "/players/" uid) player-data)
    true))

(defn list-lobbies
  "List all lobbies with 'waiting' status."
  []
  (let [raw (client/read-once "lobbies")]
    (when raw
      (->> (into {} (map (fn [[k v]]
                           [(name k) (client/read-once (str "lobbies/" (name k)))])
                         raw))
           (filter (fn [[_ v]]
                     (when v
                       (= "waiting" (get (into {} (map (fn [[k v]] [(keyword k) v]) v))
                                         :status)))))
           (into {})))))

;; ---------------------------------------------------------------------------
;; Lobby UI
;; ---------------------------------------------------------------------------

(defn- lobby-code
  "Derive a short lobby code from a Firebase push key (last 6 chars)."
  [lobby-id]
  (subs lobby-id (max 0 (- (count lobby-id) 6))))

(defn wait-for-game-start!
  "Block until the lobby transitions to 'started' and has a gameId.
   Returns the game ID, or nil on timeout/cancel."
  [lobby-id]
  (let [game-id (atom nil)
        latch   (CountDownLatch. 1)
        remove-listener
        (client/listen! (str "lobbies/" lobby-id)
          (fn [value]
            (when value
              (let [data (into {} (map (fn [[k v]] [(keyword k) v]) value))]
                (when (and (= "started" (:status data))
                           (:gameId data))
                  (reset! game-id (:gameId data))
                  (.countDown latch))))))]
    (println "\n  Waiting for all players to join...")
    (println "  (Press 'q' to cancel)\n")
    (flush)
    ;; Poll for quit key while waiting
    (let [wait-thread (Thread.
                        (fn []
                          (.await latch 300 TimeUnit/SECONDS)
                          nil))]
      (.start wait-thread)
      (loop []
        (if (.await latch 500 TimeUnit/MILLISECONDS)
          (do (remove-listener) @game-id)
          (if (and (pos? (.available System/in)) (= \q (char (.read System/in))))
            (do (remove-listener) nil)
            (recur)))))))

(defn show-lobby-menu!
  "Display the lobby menu. Returns {:action :create/:join/:back, ...}"
  []
  (println "\n  === REGICIDE ONLINE ===\n")
  (println "  1. Create game (2 players)")
  (println "  2. Create game (3 players)")
  (println "  3. Create game (4 players)")
  (println "  4. Join game")
  (println "  5. Back to main menu")
  (println)
  (print "  > ")
  (flush)
  (loop []
    (let [key (term/read-key)]
      (case key
        \1 {:action :create :num-players 2}
        \2 {:action :create :num-players 3}
        \3 {:action :create :num-players 4}
        \4 {:action :join}
        \5 {:action :back}
        (recur)))))

(defn prompt-player-name!
  "Prompt for player display name."
  []
  (print "\n  Enter your name: ")
  (flush)
  ;; Read characters until Enter (raw mode)
  (loop [chars []]
    (let [key (term/read-key)]
      (cond
        (= :enter key)
        (let [name-str (apply str chars)]
          (println)
          (if (empty? name-str) "Player" name-str))

        (= :backspace key)
        (when (seq chars)
          (print "\b \b")
          (flush)
          (recur (vec (butlast chars))))

        (char? key)
        (do (print key)
            (flush)
            (recur (conj chars key)))

        :else (recur chars)))))

(defn prompt-lobby-code!
  "Prompt for a lobby code to join."
  []
  (print "\n  Enter lobby code: ")
  (flush)
  (loop [chars []]
    (let [key (term/read-key)]
      (cond
        (= :enter key)
        (do (println)
            (apply str chars))

        (= :backspace key)
        (when (seq chars)
          (print "\b \b")
          (flush)
          (recur (vec (butlast chars))))

        (char? key)
        (do (print key)
            (flush)
            (recur (conj chars key)))

        :else (recur chars)))))
