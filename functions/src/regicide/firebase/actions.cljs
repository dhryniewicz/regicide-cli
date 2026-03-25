(ns regicide.firebase.actions
  "Action validation and dispatch. Validates incoming player actions
   against the current game state and dispatches to game.cljc functions.
   All validation happens server-side — never trust client state."
  (:require [regicide.game :as game]))

;; ---------------------------------------------------------------------------
;; Phase -> valid action types
;; ---------------------------------------------------------------------------

(def ^:private valid-actions-by-phase
  {:play-cards         #{"play-cards" "yield" "use-jester" "play-jester"}
   :suffer-damage      #{"suffer-damage" "use-jester"}
   :choose-next-player #{"choose-next-player"}})

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(defn validate-action
  "Validate an incoming action against the current game state and metadata.
   Returns nil if valid, or an error string if invalid.

   Checks:
   1. Game is still in progress
   2. Action type is valid for the current phase
   3. The action's uid matches the current player
   4. The action's version matches the current version (anti-replay)"
  [^js action state player-order version]
  (let [action-type (.-type action)
        action-uid  (.-uid action)
        action-ver  (.-version action)
        phase       (:phase state)
        current-idx (:current-player state)
        expected-uid (nth player-order current-idx)]
    (cond
      ;; Game is over
      (= :game-over phase)
      "Game is already over."

      ;; Unknown phase
      (not (contains? valid-actions-by-phase phase))
      "Invalid game phase."

      ;; Action type not valid for this phase
      (not (contains? (valid-actions-by-phase phase) action-type))
      (str "Cannot perform '" action-type "' during " (name phase) " phase.")

      ;; Wrong player
      (not= action-uid expected-uid)
      "It is not your turn."

      ;; Version mismatch (replay or stale action)
      (not= action-ver version)
      (str "Stale action: expected version " version ", got " action-ver ".")

      ;; Valid
      :else nil)))

;; ---------------------------------------------------------------------------
;; Action dispatch
;; ---------------------------------------------------------------------------

(defn- js-array->vec
  "Convert a JS array (or nil) to a Clojure vector of numbers."
  [js-arr]
  (if js-arr
    (vec (js->clj js-arr))
    []))

(defn apply-action
  "Apply a validated action to the game state.
   Returns the new state, or a map with :error if the game logic rejects it.

   Action types:
   - play-cards:         {cardIndices: [0, 2]}
   - suffer-damage:      {cardIndices: [1, 3]}
   - yield:              {}
   - use-jester:         {}
   - play-jester:        {}
   - choose-next-player: {playerIndex: 1}"
  [state ^js action]
  (let [action-type (.-type action)]
    (case action-type
      "play-cards"
      (let [indices (js-array->vec (.-cardIndices action))
            result  (game/play-cards state indices)]
        (if (:error result)
          result
          (-> result
              game/check-can-play)))

      "suffer-damage"
      (let [indices (js-array->vec (.-cardIndices action))
            result  (game/suffer-damage state indices)]
        (if (:error result)
          result
          (-> result
              game/check-can-play)))

      "yield"
      (let [result (game/yield state)]
        (if (:error result)
          result
          (game/check-can-play result)))

      "use-jester"
      (game/use-jester state)

      "play-jester"
      (game/play-jester state)

      "choose-next-player"
      (let [player-idx (.-playerIndex action)]
        (game/choose-next-player state player-idx))

      ;; Unknown action type (should be caught by validate-action)
      {:error (str "Unknown action type: " action-type)})))
