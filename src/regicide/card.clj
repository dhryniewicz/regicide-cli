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
   Ace=1, 2-10=face value, J=10, Q=15, K=20."
  [card]
  (case (:rank card)
    11 10
    12 15
    13 20
    (:rank card)))

(defn rank-label [rank]
  (case rank
    1  "A"
    11 "J"
    12 "Q"
    13 "K"
    (str rank)))

(def ^:private red "\u001b[31m")
(def ^:private reset "\u001b[0m")

(defn card-label [card]
  (let [text (str (rank-label (:rank card)) (suit-symbols (:suit card)))]
    (if (#{:hearts :diamonds} (:suit card))
      (str red text reset)
      text)))

(def suit-order {:spades 0 :hearts 1 :diamonds 2 :clubs 3})

(defn sort-by-suit
  "Sort cards by suit, then by rank within each suit."
  [cards]
  (vec (sort-by (juxt (comp suit-order :suit) :rank) cards)))

(defn sort-by-rank
  "Sort cards by rank, then by suit within each rank."
  [cards]
  (vec (sort-by (juxt :rank (comp suit-order :suit)) cards)))
