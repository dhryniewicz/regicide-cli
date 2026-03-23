(ns regicide.rules-test
  (:require [clojure.test :refer :all]
            [regicide.card :as card]
            [regicide.enemy :as enemy]
            [regicide.rules :as rules]))

(deftest valid-combo-test
  (testing "single card"
    (is (rules/valid-combo? [(card/make-card :hearts 5)])))

  (testing "pair of same rank"
    (is (rules/valid-combo? [(card/make-card :hearts 5)
                             (card/make-card :clubs 5)])))

  (testing "triple of same rank within limit"
    (is (rules/valid-combo? [(card/make-card :hearts 3)
                             (card/make-card :clubs 3)
                             (card/make-card :spades 3)])))

  (testing "pair exceeding max combo value"
    (is (not (rules/valid-combo? [(card/make-card :hearts 6)
                                  (card/make-card :clubs 6)]))))

  (testing "triple exceeding max combo value"
    (is (not (rules/valid-combo? [(card/make-card :hearts 5)
                                  (card/make-card :clubs 5)
                                  (card/make-card :spades 5)]))))

  (testing "ace + card within limit"
    (is (rules/valid-combo? [(card/make-card :hearts 1)
                             (card/make-card :clubs 7)])))

  (testing "ace + card exceeding limit"
    (is (not (rules/valid-combo? [(card/make-card :hearts 1)
                                  (card/make-card :clubs 10)]))))

  (testing "ace + pair within limit"
    (is (rules/valid-combo? [(card/make-card :hearts 1)
                             (card/make-card :clubs 4)
                             (card/make-card :spades 4)])))

  (testing "single high card is always valid"
    (is (rules/valid-combo? [(card/make-card :hearts 10)])))

  (testing "invalid - different ranks"
    (is (not (rules/valid-combo? [(card/make-card :hearts 5)
                                  (card/make-card :clubs 7)]))))

  (testing "empty is invalid"
    (is (not (rules/valid-combo? [])))))

(deftest combo-damage-test
  (is (= 5 (rules/combo-damage [(card/make-card :hearts 5)])))
  (is (= 10 (rules/combo-damage [(card/make-card :hearts 5)
                                  (card/make-card :clubs 5)])))
  (is (= 6 (rules/combo-damage [(card/make-card :hearts 1)
                                 (card/make-card :clubs 5)]))))

(deftest suit-effects-test
  (let [enemy (enemy/make-enemy (card/make-card :spades 11))] ;; Jack of Spades
    (testing "clubs double damage"
      (let [effects (rules/suit-effects [(card/make-card :clubs 5)] enemy)]
        (is (= 5 (:damage-bonus effects)))))

    (testing "spades immune on spade enemy"
      (let [effects (rules/suit-effects [(card/make-card :spades 5)] enemy)]
        (is (= 0 (:attack-reduce effects)))))

    (testing "hearts heal"
      (let [effects (rules/suit-effects [(card/make-card :hearts 3)] enemy)]
        (is (= 3 (:hearts-heal effects)))))

    (testing "diamonds draw"
      (let [effects (rules/suit-effects [(card/make-card :diamonds 4)] enemy)]
        (is (= 4 (:diamonds-draw effects)))))))

(deftest total-damage-test
  (let [enemy (enemy/make-enemy (card/make-card :hearts 11))]
    (testing "clubs doubles"
      (is (= 10 (rules/total-damage [(card/make-card :clubs 5)] enemy))))

    (testing "non-club normal damage"
      (is (= 5 (rules/total-damage [(card/make-card :spades 5)] enemy))))

    (testing "mixed combo"
      (is (= 15 (rules/total-damage [(card/make-card :clubs 5)
                                      (card/make-card :spades 5)] enemy))))))

(deftest can-absorb-damage-test
  (let [hand [(card/make-card :hearts 3)
              (card/make-card :clubs 7)]]
    (is (rules/can-absorb-damage? hand 10))
    (is (rules/can-absorb-damage? hand 9))
    (is (not (rules/can-absorb-damage? hand 11)))))
