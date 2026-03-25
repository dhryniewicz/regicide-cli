(ns regicide.game
  (:require [regicide.card :as card]
            [regicide.deck :as deck]
            [regicide.enemy :as enemy]
            [regicide.rules :as rules]))

(def hand-sizes {1 8, 2 7, 3 6, 4 5})
(def jester-counts {1 2, 2 0, 3 1, 4 2})

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
        [players tavern-after-deal]
        (reduce (fn [[players remaining] _]
                  (let [[hand remaining'] (deck/draw remaining hand-size)]
                    [(conj players {:hand hand}) remaining']))
                [[] tavern-deck]
                (range num-players))
        first-enemy (first castle-deck)
        castle-rest (vec (rest castle-deck))]
    {:castle-deck        castle-rest
     :tavern-deck        tavern-after-deal
     :discard-pile       []
     :players            (vec players)
     :current-player     0
     :current-enemy      (enemy/make-enemy first-enemy)
     :phase              :play-cards
     :status             :in-progress
     :num-players        num-players
     :sort-order         :none
     :jesters            (jester-counts num-players)
     :consecutive-yields 0
     :player-changed     false}))

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

(defn- advance-player
  "Move to the next player and set the player-changed flag."
  [state]
  (-> state
      (update :current-player #(mod (inc %) (:num-players state)))
      (assoc :player-changed true)))

;; ---------------------------------------------------------------------------
;; Core transitions
;; ---------------------------------------------------------------------------

(defn play-cards
  "Play cards from current player's hand by indices (0-based).
   Returns updated state or {:error msg}."
  [state card-indices]
  (let [hand (current-hand state)
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

            ;; 5. Apply Diamonds: distribute draws across all players starting
            ;;    with current player, total draws = diamond value, each player
            ;;    capped at hand limit
            state (if (zero? (:diamonds-draw effects))
                    state
                    (let [hand-limit (hand-sizes (:num-players state))
                          n          (:num-players state)
                          cur        (:current-player state)
                          draw-order (mapv #(mod (+ cur %) n) (range n))]
                      (first
                        (reduce
                          (fn [[st remaining-draws] player-idx]
                            (if (zero? remaining-draws)
                              [st 0]
                              (let [hand (get-in st [:players player-idx :hand])
                                    room (max 0 (- hand-limit (count hand)))
                                    draw-count (min room remaining-draws (count (:tavern-deck st)))]
                                (if (zero? draw-count)
                                  [st remaining-draws]
                                  (let [[drawn remaining] (deck/draw (:tavern-deck st) draw-count)]
                                    [(-> st
                                         (assoc :tavern-deck remaining)
                                         (update-in [:players player-idx :hand] into drawn))
                                     (- remaining-draws draw-count)])))))
                          [state (:diamonds-draw effects)]
                          draw-order))))

            ;; 6. Move played cards to discard
            state (update state :discard-pile into cards)

            ;; Reset consecutive yields (someone played)
            state (assoc state :consecutive-yields 0)]

        ;; 7. Check if enemy defeated
        (if (enemy/defeated? (:current-enemy state))
          (let [defeated-card (get-in state [:current-enemy :card])
                state (if exact-kill
                        (update state :tavern-deck #(vec (cons defeated-card %)))
                        (update state :discard-pile conj defeated-card))]
            ;; Flip next enemy or win
            (if (empty? (:castle-deck state))
              (assoc state :phase :game-over :status :won)
              (let [next-enemy-card (first (:castle-deck state))]
                (-> state
                    (assoc :castle-deck (vec (rest (:castle-deck state))))
                    (assoc :current-enemy (enemy/make-enemy next-enemy-card))
                    (assoc :phase :play-cards)
                    advance-player))))
          ;; Enemy survives - check if attack is 0 (skip damage phase)
          (if (zero? (get-in state [:current-enemy :attack]))
            (-> state
                (assoc :phase :play-cards)
                advance-player)
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
          advance-player))))

;; ---------------------------------------------------------------------------
;; Yield (multiplayer only)
;; ---------------------------------------------------------------------------

(defn yield
  "Yield: pass turn without playing. Only in multiplayer.
   Cannot yield if all other players have already yielded consecutively."
  [state]
  (cond
    (= 1 (:num-players state))
    {:error "Cannot yield in solo mode."}

    (>= (:consecutive-yields state) (dec (:num-players state)))
    {:error "Cannot yield — all other players have already yielded."}

    :else
    (-> state
        (update :consecutive-yields inc)
        (assoc :phase :play-cards)
        advance-player)))

;; ---------------------------------------------------------------------------
;; Jesters
;; ---------------------------------------------------------------------------

(defn use-jester
  "Solo jester: discard entire hand and draw a fresh hand of max size.
   Can be used before playing cards or before suffering damage.
   Does not count as drawing (bypasses diamond immunity)."
  [state]
  (if (zero? (:jesters state))
    {:error "No jesters remaining!"}
    (let [hand (current-hand state)
          hand-limit (hand-sizes (:num-players state))
          state (-> state
                    (update :discard-pile into hand)
                    (update-current-hand (constantly [])))
          draw-count (min hand-limit (count (:tavern-deck state)))
          [drawn remaining] (deck/draw (:tavern-deck state) draw-count)]
      (-> state
          (assoc :tavern-deck remaining)
          (update-current-hand into drawn)
          (update :jesters dec)))))

(defn play-jester
  "Multiplayer jester: cancel enemy immunity, deal 0 damage, skip enemy attack.
   Sets phase to :choose-next-player so the current player picks who goes next."
  [state]
  (if (zero? (:jesters state))
    {:error "No jesters remaining!"}
    (-> state
        (update :jesters dec)
        (assoc :consecutive-yields 0)
        (assoc :phase :choose-next-player))))

(defn choose-next-player
  "After a multiplayer jester, the playing player picks who goes next."
  [state player-idx]
  (if (or (neg? player-idx) (>= player-idx (:num-players state)))
    {:error "Invalid player number."}
    (-> state
        (assoc :current-player player-idx)
        (assoc :phase :play-cards)
        (assoc :player-changed true))))

;; ---------------------------------------------------------------------------
;; Checks
;; ---------------------------------------------------------------------------

(defn check-can-survive
  "Check if the current player can survive the enemy's attack.
   Not a loss if jesters are available (player can use one to refresh hand)."
  [state]
  (let [hand (current-hand state)
        attack (get-in state [:current-enemy :attack])]
    (if (and (pos? attack)
             (not (rules/can-absorb-damage? hand attack))
             (zero? (:jesters state)))
      (assoc state :status :lost :phase :game-over)
      state)))

(defn check-can-play
  "Check if the current player can act.
   Not a loss if jesters are available (player can use one to refresh hand).
   Multiplayer with empty hand = auto-yield (or loss if all stuck and no jesters)."
  [state]
  (if (and (= :play-cards (:phase state))
           (empty? (current-hand state)))
    (if (pos? (:jesters state))
      ;; Has jesters — don't auto-lose or auto-yield, let player use jester
      state
      (if (= 1 (:num-players state))
        (assoc state :status :lost :phase :game-over)
        ;; Multiplayer: auto-yield
        (let [result (yield state)]
          (if (:error result)
            (assoc state :status :lost :phase :game-over)
            (assoc result :auto-yielded true)))))
    state))
