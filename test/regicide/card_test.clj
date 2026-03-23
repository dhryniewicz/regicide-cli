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
  (is (= "A\u2665" (card/card-label (card/make-card :hearts 1))))
  (is (= "10\u2660" (card/card-label (card/make-card :spades 10))))
  (is (= "K\u2663" (card/card-label (card/make-card :clubs 13)))))

(deftest predicates-test
  (is (card/player-card? (card/make-card :hearts 10)))
  (is (not (card/player-card? (card/make-card :hearts 11))))
  (is (card/face-card? (card/make-card :spades 11)))
  (is (card/face-card? (card/make-card :spades 13)))
  (is (not (card/face-card? (card/make-card :spades 10)))))
