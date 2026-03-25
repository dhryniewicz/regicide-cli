(ns regicide.firebase.handler
  "Firebase Cloud Function entry points for Regicide online multiplayer.

   Exports:
   - processAction: triggered on /games/{gameId}/actions/{actionId} onCreate
   - createGame:    triggered on /lobbies/{lobbyId} onUpdate (when all ready)
   - cleanupLobbies: scheduled function to remove stale lobbies"
  (:require [regicide.firebase.actions :as actions]
            [regicide.firebase.state :as state]
            [regicide.firebase.lobby :as lobby]
            [regicide.game :as game]))

(def functions (js/require "firebase-functions"))
(def admin    (js/require "firebase-admin"))

(.initializeApp admin)

(def db (.database admin))

;; ---------------------------------------------------------------------------
;; processAction: the core game loop Cloud Function
;; ---------------------------------------------------------------------------

(defn- write-game-state!
  "Write the new game state to Firebase RTDB atomically.
   Updates /private/fullState, /public, and each player's /hands/{uid}."
  [game-ref new-state player-order new-version]
  (let [updates #js {}]
    ;; Private full state (admin-only, stored as EDN string)
    (aset updates "private/fullState" (state/serialize-state new-state))
    ;; Public projection
    (aset updates "public" (state/project-public new-state))
    ;; Version bump
    (aset updates "meta/version" new-version)
    ;; Game status in meta (for lobby/list queries)
    (aset updates "meta/status"
          (if (= :game-over (:phase new-state))
            (name (:status new-state))
            "active"))
    ;; Per-player hands
    (doseq [[i uid] (map-indexed vector player-order)]
      (aset updates (str "hands/" uid) (state/project-hand new-state i)))
    (.update game-ref updates)))

(defn- write-error!
  "Write an error message to /games/{gameId}/errors/{uid} for the acting player."
  [^js game-ref uid error-msg]
  (-> game-ref
      (.child (str "errors/" uid))
      (.set #js {:message   error-msg
                 :timestamp (.now js/Date)})))

(def process-action
  "Cloud Function triggered when a player writes an action to
   /games/{gameId}/actions/{actionId}. Validates and applies the action."
  (-> (.. functions -database (ref "games/{gameId}/actions/{actionId}"))
      (.onCreate
        (fn [^js snapshot ^js context]
          (let [action     (.val snapshot)
                game-id    (.. context -params -gameId)
                action-id  (.. context -params -actionId)
                game-ref   (.ref db (str "games/" game-id))]
            ;; Use a promise chain for async operations
            (-> (.once (.child game-ref "meta") "value")
                (.then
                  (fn [^js meta-snap]
                    (let [^js meta-data (.val meta-snap)
                          version    (.-version meta-data)
                          player-order (vec (js->clj (.-playerOrder meta-data)))]
                      ;; Read full state
                      (-> (.once (.child game-ref "private/fullState") "value")
                          (.then
                            (fn [^js state-snap]
                              (let [full-state (state/deserialize-state (.val state-snap))
                                    ;; Validate the action
                                    error (actions/validate-action action full-state player-order version)]
                                (if error
                                  ;; Validation failed — write error, delete action
                                  (-> (write-error! game-ref (.-uid ^js action) error)
                                      (.then #(-> ^js snapshot .-ref .remove)))
                                  ;; Validation passed — apply the action
                                  (let [result (actions/apply-action full-state action)]
                                    (if (:error result)
                                      ;; Game logic rejected the action
                                      (-> (write-error! game-ref (.-uid ^js action) (:error result))
                                          (.then #(-> ^js snapshot .-ref .remove)))
                                      ;; Success — write new state
                                      (let [new-version (inc version)
                                            ;; Run check-can-survive if entering suffer-damage
                                            final-state (if (= :suffer-damage (:phase result))
                                                          (game/check-can-survive result)
                                                          result)]
                                        (-> (write-game-state! game-ref final-state player-order new-version)
                                            (.then #(-> ^js snapshot .-ref .remove))))))))))))))))))))

;; ---------------------------------------------------------------------------
;; createGame: triggered when a lobby has all players ready
;; ---------------------------------------------------------------------------

(def create-game
  "Cloud Function triggered when a lobby is updated.
   If all players are ready, creates a new game and updates the lobby."
  (-> (.. functions -database (ref "lobbies/{lobbyId}"))
      (.onUpdate
        (fn [^js change ^js context]
          (let [^js lobby-data (.val (.-after change))
                lobby-id   (.. context -params -lobbyId)
                status     (.-status lobby-data)]
            ;; Only process if lobby is in 'waiting' state and all players ready
            (if (and (= "waiting" status)
                     (lobby/all-players-ready? lobby-data))
              ;; Create the game
              (let [players-js  (.-players ^js lobby-data)
                    players-clj (js->clj players-js :keywordize-keys false)
                    ;; Collect uids and names in join order
                    player-uids  (vec (keys players-clj))
                    player-names (mapv #(get % "name") (vals players-clj))
                    ;; Generate game data
                    game-data   (lobby/create-initial-game-data player-uids player-names)
                    ;; Create a new game node
                    game-ref    (.push (.ref db "games"))
                    game-id     (.-key game-ref)
                    ;; Build the full game write
                    game-writes #js {}]
                (aset game-writes "private/fullState" (:full-state game-data))
                (aset game-writes "public" (:public game-data))
                (aset game-writes "meta" (:meta game-data))
                (doseq [[uid hand-js] (:hands game-data)]
                  (aset game-writes (str "hands/" uid) hand-js))
                ;; Write game, then update lobby with game ID
                (-> (.set game-ref game-writes)
                    (.then #(-> (.ref db (str "lobbies/" lobby-id))
                                (.update #js {:status "started"
                                              :gameId game-id})))))
              ;; Not ready yet — no-op
              (js/Promise.resolve nil)))))))

;; ---------------------------------------------------------------------------
;; cleanupLobbies: scheduled cleanup of stale lobbies
;; ---------------------------------------------------------------------------

(def cleanup-lobbies
  "Scheduled Cloud Function that runs daily to remove lobbies older than 24 hours."
  (-> (.. functions -pubsub (schedule "every 24 hours"))
      (.onRun
        (fn [_context]
          (let [cutoff     (- (.now js/Date) (* 24 60 60 1000))
                lobbies-ref (.ref db "lobbies")]
            (-> (.once (.orderByChild lobbies-ref "createdAt")
                       (.endAt cutoff)
                       "value")
                (.then
                  (fn [^js snapshot]
                    (let [updates #js {}]
                      (.forEach snapshot
                        (fn [^js child]
                          (aset updates (.-key child) nil)))
                      (when (pos? (.-numChildren snapshot))
                        (.update lobbies-ref updates)))))))))))
