(ns regicide.firebase.state
  "State serialization, projection, and reconstruction for Firebase RTDB.
   Handles converting between full game state (stored as EDN in /private)
   and the per-player projected views sent to /public and /hands."
  (:require [cljs.reader :as reader]))

;; ---------------------------------------------------------------------------
;; Serialization: full game state <-> EDN string
;; ---------------------------------------------------------------------------

(defn serialize-state
  "Serialize a full game state map to an EDN string for storage in /private/fullState."
  [state]
  (pr-str state))

(defn deserialize-state
  "Deserialize an EDN string from /private/fullState back into a game state map."
  [edn-str]
  (reader/read-string edn-str))

;; ---------------------------------------------------------------------------
;; Card serialization for Firebase RTDB (JSON-compatible)
;; ---------------------------------------------------------------------------

(defn card->js
  "Convert a Clojure card map to a JSON-compatible JS object."
  [{:keys [suit rank]}]
  #js {:suit (name suit) :rank rank})

(defn cards->js
  "Convert a sequence of cards to a JS array."
  [cards]
  (to-array (map card->js cards)))

;; ---------------------------------------------------------------------------
;; Enemy serialization
;; ---------------------------------------------------------------------------

(defn enemy->js
  "Convert the current enemy map to a JSON-compatible JS object."
  [{:keys [card health attack max-health max-attack]}]
  #js {:suit      (name (:suit card))
       :rank      (:rank card)
       :health    health
       :attack    attack
       :maxHealth max-health
       :maxAttack max-attack})

;; ---------------------------------------------------------------------------
;; Public state projection
;; ---------------------------------------------------------------------------

(defn project-public
  "Project the full game state into the public view that all game participants can see.
   Contains no hidden information — only counts for decks, not contents."
  [state]
  (let [players (:players state)]
    #js {:currentEnemy      (enemy->js (:current-enemy state))
         :castleDeckCount   (count (:castle-deck state))
         :tavernDeckCount   (count (:tavern-deck state))
         :discardPileCount  (count (:discard-pile state))
         :currentPlayer     (:current-player state)
         :phase             (name (:phase state))
         :status            (name (:status state))
         :jesters           (:jesters state)
         :consecutiveYields (:consecutive-yields state)
         :numPlayers        (:num-players state)
         :handSizes         (clj->js (into {}
                              (map-indexed (fn [i p] [(str i) (count (:hand p))])
                                           players)))}))

;; ---------------------------------------------------------------------------
;; Per-player hand projection
;; ---------------------------------------------------------------------------

(defn project-hand
  "Extract a specific player's hand as a JS array of card objects.
   Only this player's uid should be able to read their hand node."
  [state player-index]
  (cards->js (get-in state [:players player-index :hand])))

(defn project-all-hands
  "Return a map of player-index -> JS hand array for all players.
   Caller maps player indices to uids before writing to Firebase."
  [state]
  (into {}
    (map-indexed (fn [i player]
                   [i (cards->js (:hand player))])
                 (:players state))))
