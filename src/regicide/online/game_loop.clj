(ns regicide.online.game-loop
  "Online game loop for networked multiplayer.
   Replaces the local game loop with a Firebase-backed action/listen cycle.
   Reuses existing display and terminal modules."
  (:require [regicide.online.client :as client]
            [regicide.card :as card]
            [regicide.game :as game]
            [regicide.rules :as rules]
            [regicide.cli.display :as display]
            [regicide.cli.terminal :as term])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

;; ---------------------------------------------------------------------------
;; State reconstruction from Firebase data
;; ---------------------------------------------------------------------------

(defn- js-card->clj
  "Convert a Firebase card map {:suit \"spades\" :rank 5} to game card."
  [{:keys [suit rank]}]
  {:suit (keyword suit) :rank rank})

(defn- reconstruct-enemy
  "Reconstruct the enemy map from Firebase public data."
  [{:keys [suit rank health attack maxHealth maxAttack]}]
  {:card       {:suit (keyword suit) :rank rank}
   :health     health
   :attack     attack
   :max-health maxHealth
   :max-attack maxAttack})

(defn build-display-state
  "Build a state map compatible with cli/display from Firebase public data + player hand.
   The display layer uses this to render the game without modification."
  [public-data my-hand my-player-index]
  (let [hand-sizes (:handSizes public-data)
        num-players (or (:numPlayers public-data) (count hand-sizes))
        players (vec
                  (for [i (range num-players)]
                    (if (= i my-player-index)
                      {:hand (mapv js-card->clj my-hand)}
                      ;; Other players — fake hand with correct count for display
                      (let [size (get hand-sizes (keyword (str i)) 0)]
                        {:hand (vec (repeat size {:suit :spades :rank 0}))}))))]
    {:castle-deck     (vec (repeat (:castleDeckCount public-data) nil))
     :tavern-deck     (vec (repeat (:tavernDeckCount public-data) nil))
     :discard-pile    (vec (repeat (:discardPileCount public-data) nil))
     :players         players
     :current-player  (:currentPlayer public-data)
     :current-enemy   (reconstruct-enemy (:currentEnemy public-data))
     :phase           (keyword (:phase public-data))
     :status          (keyword (:status public-data))
     :num-players     num-players
     :jesters         (:jesters public-data)
     :consecutive-yields (:consecutiveYields public-data)
     :sort-order      :none
     :player-changed  false}))

;; ---------------------------------------------------------------------------
;; Waiting screen
;; ---------------------------------------------------------------------------

(defn- render-waiting-screen
  "Show a waiting screen when it's another player's turn."
  [state meta-data]
  (let [current-idx (:current-player state)
        player-names (:playerNames meta-data)
        player-order (:playerOrder meta-data)
        current-uid (nth player-order current-idx)
        current-name (get player-names (keyword current-uid) (str "Player " (inc current-idx)))]
    (print term/clear-screen)
    (println (display/render-game-state state))
    (println (str "\n  Waiting for " current-name "'s turn..."))
    (flush)))

;; ---------------------------------------------------------------------------
;; Action sending
;; ---------------------------------------------------------------------------

(defn- send-game-action!
  "Send an action to Firebase and wait for the state to update."
  [game-id uid version action-map]
  (let [full-action (merge {:uid uid :version version} action-map)]
    (client/send-action! game-id full-action)))

;; ---------------------------------------------------------------------------
;; Card selection (reused from CLI)
;; ---------------------------------------------------------------------------

(def ^:private sort-cycle {:none :suit, :suit :rank, :rank :none})

(defn- show-error! [state msg]
  (print term/clear-screen)
  (println (display/render-game-state state))
  (println (str "\n  " msg))
  (println "\n  Press any key...")
  (flush)
  (term/read-key))

(defn- redraw! [state selector-state action-info phase]
  (print term/clear-screen)
  (println (display/render-game-state state selector-state))
  (when action-info
    (println (display/render-action-result action-info)))
  (println (display/render-selector-prompt phase true))
  (flush))

(defn- confirm-quit? []
  (println "\n  Quit game? (y/n)")
  (flush)
  (loop []
    (let [key (term/read-key)]
      (cond
        (or (= key \y) (= key \Y)) true
        (or (= key \n) (= key \N) (= key :escape)) false
        :else (recur)))))

(defn- card-select-loop
  "Interactive card selection. Returns selected indices as vec, or a keyword command."
  [state phase]
  (let [hand (game/current-hand state)
        hand-size (count hand)]
    (when (pos? hand-size)
      (loop [cursor 0
             selected #{}]
        (redraw! state {:cursor cursor :selected selected} nil phase)
        (let [key (term/read-key)]
          (case key
            :left  (recur (mod (dec cursor) hand-size) selected)
            :right (recur (mod (inc cursor) hand-size) selected)
            :up    (let [display-cards (display/sorted-hand-with-indices hand (or (:sort-order state) :none))
                         [orig-idx _] (nth display-cards cursor)
                         new-selected (if (contains? selected orig-idx)
                                        (disj selected orig-idx)
                                        (conj selected orig-idx))]
                     (recur cursor new-selected))
            :enter (if (empty? selected)
                     (recur cursor selected)
                     (vec selected))
            (:escape :down) (recur cursor selected)
            (cond
              (= key \p) :sort
              (= key \j) :jester
              (and (= key \y) (= phase :play-cards)) :yield
              (= key \q) (if (confirm-quit?) :quit (recur cursor selected))
              (or (= key \h) (= key \?)) :help
              :else (recur cursor selected))))))))

;; ---------------------------------------------------------------------------
;; Player chooser for jester
;; ---------------------------------------------------------------------------

(defn- player-chooser-loop!
  "Let the current player choose who goes next (for multiplayer jester)."
  [state]
  (print term/clear-screen)
  (println (display/render-game-state state))
  (println (display/render-player-chooser state))
  (flush)
  (loop []
    (let [key (term/read-key)]
      (if (and (char? key) (Character/isDigit ^char key))
        (let [idx (dec (Character/digit ^char key 10))]
          (if (and (>= idx 0)
                   (< idx (:num-players state))
                   (not= idx (:current-player state)))
            idx
            (recur)))
        (recur)))))

;; ---------------------------------------------------------------------------
;; Main online game loop
;; ---------------------------------------------------------------------------

(defn- poll-state!
  "Read the current game state from Firebase."
  [game-id uid]
  (let [public-data (client/read-public-state game-id)
        hand-data   (client/read-hand game-id uid)
        meta-data   (client/read-meta game-id)]
    {:public public-data
     :hand   hand-data
     :meta   meta-data}))

(defn- my-player-index
  "Find this player's index in the player order."
  [meta-data uid]
  (let [order (:playerOrder meta-data)]
    (first (keep-indexed (fn [i u] (when (= u uid) i)) order))))

(defn run-game-loop!
  "Main online game loop. Polls Firebase for state and handles player turns."
  [game-id uid]
  (println "\n  Game started! Loading...")
  (flush)
  (loop [sort-order :none
         prev-version -1]
    (let [{:keys [public hand meta]} (poll-state! game-id uid)
          version     (:version meta)
          my-idx      (my-player-index meta uid)
          state       (-> (build-display-state public hand my-idx)
                          (assoc :sort-order sort-order))
          my-turn?    (= my-idx (:current-player state))
          phase       (:phase state)]

      (cond
        ;; Game over
        (or (= :game-over phase) (#{:won :lost} (:status state)))
        (do (print term/clear-screen)
            (println (display/render-game-state state))
            (println (display/render-game-over state)))

        ;; Not my turn — show waiting screen, then poll again
        (not my-turn?)
        (do (render-waiting-screen state meta)
            (Thread/sleep 1000)
            (recur sort-order version))

        ;; My turn — play phase
        (= :play-cards phase)
        (let [hand-cards (game/current-hand state)]
          (if (empty? hand-cards)
            (if (pos? (:jesters state))
              ;; Empty hand, has jester — wait for j or q
              (let [action (do (print term/clear-screen)
                               (println (display/render-game-state state))
                               (println "\n  No cards in hand! Press j to use a jester, or q to quit.")
                               (flush)
                               (loop []
                                 (let [key (term/read-key)]
                                   (cond
                                     (= key \j) :jester
                                     (= key \q) (if (confirm-quit?) :quit (recur))
                                     :else (recur)))))]
                (when (= :jester action)
                  (send-game-action! game-id uid version {:type "use-jester"})
                  (Thread/sleep 500))
                (when-not (= :quit action)
                  (recur sort-order version)))
              ;; Empty hand, no jester — auto-yield handled server-side
              (do (Thread/sleep 500)
                  (recur sort-order version)))
            ;; Has cards — normal card selection
            (let [result (card-select-loop state :play-cards)]
              (cond
                (nil? result) nil
                (= :quit result) nil
                (= :sort result) (recur (sort-cycle sort-order) version)
                (= :yield result)
                (do (send-game-action! game-id uid version {:type "yield"})
                    (Thread/sleep 500)
                    ;; Check for errors
                    (when-let [err (client/read-error game-id uid)]
                      (show-error! state err))
                    (recur sort-order version))
                (= :jester result)
                (do (send-game-action! game-id uid version {:type "play-jester"})
                    (Thread/sleep 500)
                    (when-let [err (client/read-error game-id uid)]
                      (show-error! state err))
                    (recur sort-order version))
                (= :help result)
                (do (print term/clear-screen)
                    (println (display/render-help))
                    (term/read-key)
                    (recur sort-order version))
                :else
                (do (send-game-action! game-id uid version
                      {:type "play-cards" :cardIndices result})
                    (Thread/sleep 500)
                    (when-let [err (client/read-error game-id uid)]
                      (show-error! state err))
                    (recur sort-order version))))))

        ;; My turn — suffer damage
        (= :suffer-damage phase)
        (let [result (card-select-loop state :suffer-damage)]
          (cond
            (nil? result) nil
            (= :quit result) nil
            (= :sort result) (recur (sort-cycle sort-order) version)
            (= :jester result)
            (do (send-game-action! game-id uid version {:type "use-jester"})
                (Thread/sleep 500)
                (when-let [err (client/read-error game-id uid)]
                  (show-error! state err))
                (recur sort-order version))
            (= :help result)
            (do (print term/clear-screen)
                (println (display/render-help))
                (term/read-key)
                (recur sort-order version))
            :else
            (do (send-game-action! game-id uid version
                  {:type "suffer-damage" :cardIndices result})
                (Thread/sleep 500)
                (when-let [err (client/read-error game-id uid)]
                  (show-error! state err))
                (recur sort-order version))))

        ;; My turn — choose next player (after jester)
        (= :choose-next-player phase)
        (let [idx (player-chooser-loop! state)]
          (send-game-action! game-id uid version
            {:type "choose-next-player" :playerIndex idx})
          (Thread/sleep 500)
          (when-let [err (client/read-error game-id uid)]
            (show-error! state err))
          (recur sort-order version))

        ;; Unknown phase — poll again
        :else
        (do (Thread/sleep 1000)
            (recur sort-order version))))))
