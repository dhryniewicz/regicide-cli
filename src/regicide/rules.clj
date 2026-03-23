(ns regicide.rules
  (:require [regicide.card :as card]
            [regicide.enemy :as enemy]))

(def ^:private max-same-rank-value
  "Maximum total value for same-rank combos (pairs/triples/quads)."
  10)

(defn valid-combo?
  "Validates a set of cards as a legal play.
   - Single card: always valid
   - Ace companion: exactly one ace + one non-ace card, no value limit
   - Same-rank combo: pair/triple/quad of same rank, total value <= 10"
  [cards]
  (when (seq cards)
    (let [ranks (map :rank cards)
          non-aces (remove #{1} ranks)
          ace-count (count (filter #{1} ranks))]
      (cond
        ;; Single card
        (= 1 (count cards)) true
        ;; Ace companion: one ace + one non-ace (no value limit)
        (and (= 1 ace-count) (= 1 (count non-aces))) true
        ;; Multiple aces (same rank combo, always <= 10)
        (and (pos? ace-count) (empty? non-aces)) true
        ;; Same-rank combo: pair/triple/quad with value limit
        (and (zero? ace-count)
             (apply = ranks)
             (<= (reduce + (map card/card-value cards)) max-same-rank-value)) true
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
