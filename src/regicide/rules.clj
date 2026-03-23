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

(defn combo-value
  "Total played value of a combo (sum of card values)."
  [cards]
  (reduce + (map card/card-value cards)))

(defn- active-suits
  "Returns the set of suits present in the combo that the enemy is NOT immune to."
  [cards current-enemy]
  (into #{}
    (remove #(enemy/immune-to-suit? current-enemy %)
            (map :suit cards))))

(defn suit-effects
  "Compute suit power effects. Each active suit uses the full combo value.
   Returns {:clubs? bool :attack-reduce N :hearts-heal N :diamonds-draw N}"
  [cards current-enemy]
  (let [value (combo-value cards)
        suits (active-suits cards current-enemy)]
    {:clubs?        (contains? suits :clubs)
     :attack-reduce (if (contains? suits :spades)   value 0)
     :hearts-heal   (if (contains? suits :hearts)   value 0)
     :diamonds-draw (if (contains? suits :diamonds) value 0)}))

(defn total-damage
  "Total damage dealt by a combo. Clubs doubles the full combo value."
  [cards current-enemy]
  (let [value (combo-value cards)
        effects (suit-effects cards current-enemy)]
    (if (:clubs? effects) (* 2 value) value)))

(defn can-absorb-damage?
  "True if the hand has enough total card value to absorb the given attack."
  [hand attack]
  (>= (reduce + 0 (map card/card-value hand)) attack))

(defn can-play?
  "True if the player has at least one card to play."
  [hand]
  (pos? (count hand)))
