(ns regicide.game
  (:require [regicide.card :as card]
            [regicide.deck :as deck]
            [regicide.enemy :as enemy]
            [regicide.rules :as rules]))

(def hand-sizes {1 8, 2 7, 3 6, 4 5})

;; ---------------------------------------------------------------------------
;; Setup
;; ---------------------------------------------------------------------------

(defn- build-castle-deck
  "Build the castle deck: shuffled Jacks, then Queens, then Kings."
  [face-cards]
  (let [by-rank (group-by :rank face-cards)]
    (vec (concat (shuffle (by-rank 11))
                 (shuffle (by-rank 12))
                 (shuffle (by-rank 13))))))

(defn new-game
  "Create a new game state for the given number of players (1-4)."
  [num-players]
  (let [all-cards (card/make-deck)
        face-cards (filterv card/face-card? all-cards)
        player-cards (filterv card/player-card? all-cards)
        castle-deck (build-castle-deck face-cards)
        tavern-deck (deck/shuffle-deck player-cards)
        hand-size (hand-sizes num-players)
        ;; Deal hands to each player
        [players tavern-after-deal]
        (reduce (fn [[players remaining] _]
                  (let [[hand remaining'] (deck/draw remaining hand-size)]
                    [(conj players {:hand hand}) remaining']))
                [[] tavern-deck]
                (range num-players))
        ;; Flip first enemy
        first-enemy (first castle-deck)
        castle-rest (vec (rest castle-deck))]
    {:castle-deck    castle-rest
     :tavern-deck    tavern-after-deal
     :discard-pile   []
     :players        (vec players)
     :current-player 0
     :current-enemy  (enemy/make-enemy first-enemy)
     :phase          :play-cards
     :status         :in-progress
     :num-players    num-players}))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn current-hand [state]
  (get-in state [:players (:current-player state) :hand]))

(defn- update-current-hand [state f & args]
  (apply update-in state [:players (:current-player state) :hand] f args))

(defn- remove-indices
  "Remove elements at the given indices from a vector."
  [v indices]
  (let [idx-set (set indices)]
    (vec (keep-indexed (fn [i x] (when-not (idx-set i) x)) v))))

(defn- next-player [state]
  (update state :current-player #(mod (inc %) (:num-players state))))

;; ---------------------------------------------------------------------------
;; Core transitions
;; ---------------------------------------------------------------------------

(defn play-cards
  "Play cards from current player's hand by indices (0-based).
   Returns updated state or {:error msg}."
  [state card-indices]
  (let [hand (current-hand state)
        ;; Validate indices
        _ (when (some #(or (neg? %) (>= % (count hand))) card-indices)
            (throw (ex-info "Invalid card index" {})))
        cards (mapv hand card-indices)]
    (if-not (rules/valid-combo? cards)
      {:error "Invalid combo. Cards must be the same rank, or Ace + card(s) of the same rank."}
      (let [current-enemy (:current-enemy state)
            effects (rules/suit-effects cards current-enemy)
            damage (rules/total-damage cards current-enemy)

            ;; 1. Remove cards from hand
            state (update-current-hand state remove-indices card-indices)

            ;; 2. Apply Spades: reduce enemy attack
            state (update state :current-enemy enemy/reduce-attack (:attack-reduce effects))

            ;; 3. Apply Hearts: shuffle cards from discard into tavern
            state (let [heal-count (min (:hearts-heal effects) (count (:discard-pile state)))
                        healed (vec (take heal-count (:discard-pile state)))
                        remaining-discard (vec (drop heal-count (:discard-pile state)))]
                    (-> state
                        (assoc :discard-pile remaining-discard)
                        (update :tavern-deck #(deck/shuffle-deck (vec (concat % healed))))))

            ;; 4. Apply damage to enemy
            exact-kill (enemy/exact-kill? current-enemy damage)
            state (update state :current-enemy enemy/apply-damage damage)

            ;; 5. Apply Diamonds: draw cards into current player's hand
            state (let [draw-count (min (:diamonds-draw effects) (count (:tavern-deck state)))
                        [drawn remaining] (deck/draw (:tavern-deck state) draw-count)]
                    (-> state
                        (assoc :tavern-deck remaining)
                        (update-current-hand into drawn)))

            ;; 6. Move played cards to discard
            state (update state :discard-pile into cards)]

        ;; 7. Check if enemy defeated
        (if (enemy/defeated? (:current-enemy state))
          (let [defeated-card (get-in state [:current-enemy :card])
                ;; Remove defeated enemy card from discard (it was added with played cards? No - it wasn't)
                ;; The defeated enemy card needs to go to hand (exact kill) or discard
                state (if exact-kill
                        (update-current-hand state conj defeated-card)
                        (update state :discard-pile conj defeated-card))]
            ;; Flip next enemy or win
            (if (empty? (:castle-deck state))
              (assoc state :phase :game-over :status :won)
              (let [next-enemy-card (first (:castle-deck state))]
                (-> state
                    (assoc :castle-deck (vec (rest (:castle-deck state))))
                    (assoc :current-enemy (enemy/make-enemy next-enemy-card))
                    (assoc :phase :play-cards)
                    next-player))))
          ;; Enemy survives - check if attack is 0 (skip damage phase)
          (if (zero? (get-in state [:current-enemy :attack]))
            (-> state
                (assoc :phase :play-cards)
                next-player)
            (assoc state :phase :suffer-damage)))))))

(defn suffer-damage
  "Discard cards from current player's hand to absorb enemy attack.
   card-indices are 0-based indices into the player's hand.
   Returns updated state or {:error msg}."
  [state card-indices]
  (let [hand (current-hand state)
        cards (mapv hand card-indices)
        total-value (reduce + (map card/card-value cards))
        attack (get-in state [:current-enemy :attack])]
    (if (< total-value attack)
      {:error (str "Not enough! You need to discard at least " attack
                   " worth of cards. You selected " total-value ".")}
      (-> state
          (update-current-hand remove-indices card-indices)
          (update :discard-pile into cards)
          (assoc :phase :play-cards)
          next-player))))

(defn check-can-survive
  "Check if the current player can survive the enemy's attack.
   Returns the state with :status :lost if they cannot."
  [state]
  (let [hand (current-hand state)
        attack (get-in state [:current-enemy :attack])]
    (if (and (pos? attack)
             (not (rules/can-absorb-damage? hand attack)))
      (assoc state :status :lost :phase :game-over)
      state)))

(defn check-can-play
  "Check if the current player has any cards to play.
   In solo mode with no cards and no ability to yield, it's a loss."
  [state]
  (if (and (= :play-cards (:phase state))
           (empty? (current-hand state))
           (= 1 (:num-players state)))
    (assoc state :status :lost :phase :game-over)
    state))
