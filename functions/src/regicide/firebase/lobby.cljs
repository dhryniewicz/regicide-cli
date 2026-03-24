(ns regicide.firebase.lobby
  "Lobby lifecycle management. Handles game creation when all players
   in a lobby are ready."
  (:require [regicide.game :as game]
            [regicide.firebase.state :as state]))

(defn all-players-ready?
  "Check if all players in the lobby are ready and the lobby has enough players."
  [lobby-data]
  (let [num-players (.-numPlayers lobby-data)
        players     (js->clj (.-players lobby-data) :keywordize-keys false)
        player-list (vals players)]
    (and (= (count player-list) num-players)
         (every? #(get % "ready") player-list))))

(defn create-initial-game-data
  "Create the initial Firebase data for a new game.
   Returns a map with :full-state (EDN), :public (JS), :hands (map of uid->JS),
   and :meta (JS) ready to write to Firebase.

   player-uids is a vector of uid strings in player order."
  [player-uids player-names]
  (let [num-players (count player-uids)
        game-state  (game/new-game num-players)
        full-state  (state/serialize-state game-state)
        public      (state/project-public game-state)
        hands       (into {}
                      (map-indexed (fn [i uid]
                                     [uid (state/project-hand game-state i)])
                                   player-uids))
        ;; playerUids is an object for security rule .exists() checks
        player-uids-map (into {} (map (fn [uid] [uid true]) player-uids))
        meta-data   (clj->js {:playerOrder player-uids
                               :playerNames (zipmap player-uids player-names)
                               :playerUids  player-uids-map
                               :version     0
                               :numPlayers  num-players
                               :status      "active"})]
    {:full-state full-state
     :public     public
     :hands      hands
     :meta       meta-data}))
