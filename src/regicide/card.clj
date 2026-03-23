(ns regicide.card)

(def suits [:spades :hearts :diamonds :clubs])

(def suit-symbols
  {:spades   "\u2660"
   :hearts   "\u2665"
   :diamonds "\u2666"
   :clubs    "\u2663"})

(defn make-card [suit rank]
  {:suit suit :rank rank})

(defn make-deck []
  (vec (for [suit suits
             rank (range 1 14)]
         (make-card suit rank))))

(defn face-card? [card]
  (>= (:rank card) 11))

(defn player-card? [card]
  (<= (:rank card) 10))

(defn card-value
  "Returns the numeric value of a card for damage/discard purposes.
   Ace=1, 2-10=face value."
  [card]
  (:rank card))

(defn rank-label [rank]
  (case rank
    1  "A"
    11 "J"
    12 "Q"
    13 "K"
    (str rank)))

(defn card-label [card]
  (str (rank-label (:rank card)) (suit-symbols (:suit card))))
