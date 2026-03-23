(ns regicide.deck)

(defn shuffle-deck [cards]
  (vec (shuffle cards)))

(defn draw
  "Draw n cards from top of deck. Returns [drawn remaining]."
  [deck n]
  (let [n (min n (count deck))]
    [(vec (take n deck))
     (vec (drop n deck))]))

(defn add-to-bottom [deck cards]
  (vec (concat deck cards)))
