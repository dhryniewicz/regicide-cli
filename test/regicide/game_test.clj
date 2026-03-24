(ns regicide.game-test
  (:require [clojure.test :refer :all]
            [regicide.card :as card]
            [regicide.enemy :as enemy]
            [regicide.game :as game]))

(deftest new-game-test
  (let [state (game/new-game 1)]
    (is (= :in-progress (:status state)))
    (is (= :play-cards (:phase state)))
    (is (= 1 (:num-players state)))
    (is (= 8 (count (game/current-hand state))) "solo player gets 8 cards")
    (is (= 11 (count (:castle-deck state))) "11 enemies remaining after first flip")
    (is (= 32 (count (:tavern-deck state))) "40 - 8 = 32 in tavern")
    (is (some? (:current-enemy state)))
    (is (= 11 (get-in state [:current-enemy :card :rank])) "first enemy is a Jack")))

(deftest play-cards-basic-test
  (let [state (game/new-game 1)
        ;; Play the first card from hand
        hand (game/current-hand state)
        first-card (first hand)
        result (game/play-cards state [0])]
    (is (not (:error result)) "should be a valid play")
    (is (<= (count (game/current-hand result))
            (+ 7 (card/card-value first-card))) "hand size should account for play and diamond draws")
    ;; The played card's damage should have been applied
    (is (< (get-in result [:current-enemy :health])
           (get-in state [:current-enemy :max-health])))))

(deftest play-invalid-combo-test
  (let [state (game/new-game 1)
        hand (game/current-hand state)
        ;; Find two cards with different ranks (non-ace)
        non-aces (filter #(not= 1 (:rank %)) hand)
        diff-ranks (take 2 (filter #(not= (:rank (first non-aces)) (:rank %))
                                    (rest non-aces)))]
    (when (>= (count diff-ranks) 1)
      (let [idx1 (.indexOf (vec hand) (first non-aces))
            idx2 (.indexOf (vec hand) (first diff-ranks))
            result (game/play-cards state [idx1 idx2])]
        (is (:error result) "different ranks should be invalid combo")))))

(deftest suffer-damage-test
  ;; Create a state that's in suffer-damage phase
  (let [state (game/new-game 1)
        ;; Play first card to get to damage phase
        result (game/play-cards state [0])]
    (when (= :suffer-damage (:phase result))
      (let [hand (game/current-hand result)
            attack (get-in result [:current-enemy :attack])
            ;; Find enough cards to absorb
            total 0
            indices (loop [i 0 t 0 acc []]
                      (if (or (>= t attack) (>= i (count hand)))
                        acc
                        (recur (inc i)
                               (+ t (card/card-value (hand i)))
                               (conj acc i))))
            discard-result (game/suffer-damage result indices)]
        (is (not (:error discard-result)))
        (is (= :play-cards (:phase discard-result)))))))

(deftest check-can-survive-test
  (let [state {:current-enemy {:attack 100}
               :players [{:hand [(card/make-card :hearts 2)]}]
               :current-player 0
               :status :in-progress}
        result (game/check-can-survive state)]
    (is (= :lost (:status result)))))

(deftest use-jester-test
  (let [state (game/new-game 1)]
    (testing "jester discards hand and draws fresh"
      (let [old-hand (game/current-hand state)
            result (game/use-jester state)]
        (is (not (:error result)))
        (is (= 2 (:jesters state)))
        (is (= 1 (:jesters result)))
        (is (= 8 (count (game/current-hand result))))))

    (testing "no jesters remaining"
      (let [state (assoc state :jesters 0)
            result (game/use-jester state)]
        (is (:error result))))))
