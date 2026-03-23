(ns regicide.rules
  (:require [regicide.card :as card]
            [regicide.enemy :as enemy]))

(def ^:private max-combo-value
  "Maximum total value of a multi-card combo."
  10)

(defn valid-combo?
  "Validates a set of cards as a legal play.
   Legal combos (max total value 10 for multi-card plays):
   - Single card
   - Multiple cards of the same rank (pair/triple/quad)
   - Ace(s) + card(s) of the same rank
   - Ace + single non-ace card"
  [cards]
  (when (seq cards)
    (let [ranks (map :rank cards)
          aces (filter #{1} ranks)
          non-aces (remove #{1} ranks)
          ace-count (count aces)
          total (reduce + ranks)]
      (cond
        (= 1 (count cards)) true
        (> total max-combo-value) false
        (and (zero? ace-count) (apply = ranks)) true
        (and (pos? ace-count)
             (or (empty? non-aces)
                 (apply = non-aces))) true
        :else false))))

(defn combo-damage
  "Base damage of a combo (sum of card values, before suit modifiers)."
  [cards]
  (reduce + (map card/card-value cards)))

(defn suit-effects
  "Compute suit power effects for played cards, respecting enemy immunity.
   Returns {:damage-bonus N :attack-reduce N :hearts-heal N :diamonds-draw N}"
  [cards current-enemy]
  (reduce
    (fn [effects card]
      (if (enemy/immune-to-suit? current-enemy (:suit card))
        effects
        (let [v (card/card-value card)]
          (case (:suit card)
            :clubs    (update effects :damage-bonus + v)
            :spades   (update effects :attack-reduce + v)
            :hearts   (update effects :hearts-heal + v)
            :diamonds (update effects :diamonds-draw + v)))))
    {:damage-bonus 0 :attack-reduce 0 :hearts-heal 0 :diamonds-draw 0}
    cards))

(defn total-damage
  "Total damage dealt by a combo (base + clubs bonus)."
  [cards current-enemy]
  (let [base (combo-damage cards)
        effects (suit-effects cards current-enemy)]
    (+ base (:damage-bonus effects))))

(defn can-absorb-damage?
  "True if the hand has enough total card value to absorb the given attack."
  [hand attack]
  (>= (reduce + 0 (map card/card-value hand)) attack))

(defn can-play?
  "True if the player has at least one card to play."
  [hand]
  (pos? (count hand)))
