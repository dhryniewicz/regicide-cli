(ns regicide.card-test
  (:require [clojure.test :refer :all]
            [regicide.card :as card]))

(deftest make-deck-test
  (let [deck (card/make-deck)]
    (is (= 52 (count deck)))
    (is (= 4 (count (filter #(= 1 (:rank %)) deck))) "4 aces")
    (is (= 13 (count (filter #(= :spades (:suit %)) deck))) "13 spades")
    (is (= (count (distinct deck)) 52) "all unique")))

(deftest card-value-test
  (is (= 1 (card/card-value (card/make-card :hearts 1))))
  (is (= 5 (card/card-value (card/make-card :clubs 5))))
  (is (= 10 (card/card-value (card/make-card :spades 10)))))

(deftest card-label-test
  (is (= "\u001b[31mA\u2665\u001b[0m" (card/card-label (card/make-card :hearts 1)))
      "hearts should be red")
  (is (= "10\u2660" (card/card-label (card/make-card :spades 10)))
      "spades should not be colored")
  (is (= "K\u2663" (card/card-label (card/make-card :clubs 13)))
      "clubs should not be colored")
  (is (= "\u001b[31m5\u2666\u001b[0m" (card/card-label (card/make-card :diamonds 5)))
      "diamonds should be red"))

(deftest predicates-test
  (is (card/player-card? (card/make-card :hearts 10)))
  (is (not (card/player-card? (card/make-card :hearts 11))))
  (is (card/face-card? (card/make-card :spades 11)))
  (is (card/face-card? (card/make-card :spades 13)))
  (is (not (card/face-card? (card/make-card :spades 10)))))
