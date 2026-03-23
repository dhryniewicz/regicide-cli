(ns regicide.display-test
  (:require [clojure.test :refer :all]
            [regicide.card :as card]
            [regicide.enemy :as enemy]
            [regicide.game :as game]
            [regicide.cli.display :as display]))

;; These tests ensure all display functions can be called without errors
;; (no forward-reference or ordering issues) and produce non-empty strings.

(defn- make-test-state
  "Build a minimal game state for display tests."
  [phase]
  (-> (game/new-game 1)
      (assoc :phase phase)))

(deftest render-game-state-without-selector-test
  (let [state (make-test-state :play-cards)
        output (display/render-game-state state)]
    (is (string? output))
    (is (pos? (count output)))
    (is (re-find #"REGICIDE" output))
    (is (re-find #"Your Hand" output))
    (is (re-find #"ATTACK" output))))

(deftest render-game-state-with-selector-test
  (let [state (make-test-state :play-cards)
        selector {:cursor 0 :selected #{}}
        output (display/render-game-state state selector)]
    (is (string? output))
    (is (pos? (count output)))))

(deftest render-game-state-suffer-damage-test
  (let [state (make-test-state :suffer-damage)
        selector {:cursor 0 :selected #{1}}
        output (display/render-game-state state selector)]
    (is (string? output))
    (is (re-find #"DEFEND" output))))

(deftest render-game-state-immune-hover-test
  (let [state (make-test-state :play-cards)
        hand (game/current-hand state)
        enemy-suit (get-in state [:current-enemy :card :suit])
        ;; Find a card in hand matching the enemy's suit
        immune-idx (first (keep-indexed
                            (fn [i c] (when (= (:suit c) enemy-suit) i))
                            hand))
        ;; Find its display position (unsorted = same index)
        selector {:cursor (or immune-idx 0) :selected #{}}
        output (display/render-game-state state selector)]
    (is (string? output))
    (when immune-idx
      (is (re-find #"immune" output)
          "Should show immune warning when cursor is on matching suit"))))

(deftest render-action-result-with-active-powers-test
  (let [action {:played [(card/make-card :spades 5)]
                :damage 5
                :attack-reduce 5
                :hearts-heal 0
                :diamonds-draw 0
                :enemy-defeated false
                :exact-kill false
                :immune-suit :clubs
                :played-suits #{:spades}}
        output (display/render-action-result action)]
    (is (string? output))
    (is (re-find #"Damage dealt: 5" output))
    (is (re-find #"Reduced enemy attack" output))
    (is (not (re-find #"immune" output))
        "Should not show immune when played suit differs from enemy suit")))

(deftest render-action-result-with-cancelled-power-test
  (let [action {:played [(card/make-card :clubs 5)]
                :damage 5
                :attack-reduce 0
                :hearts-heal 0
                :diamonds-draw 0
                :enemy-defeated false
                :exact-kill false
                :immune-suit :clubs
                :played-suits #{:clubs}}
        output (display/render-action-result action)]
    (is (string? output))
    (is (re-find #"immune" output)
        "Should show immune label for cancelled power")))

(deftest render-action-result-discard-test
  (let [action {:discarded [(card/make-card :hearts 3) (card/make-card :spades 7)]}
        output (display/render-action-result action)]
    (is (string? output))
    (is (re-find #"Discarded" output))))

(deftest render-enemy-defeated-test
  (let [action {:played [(card/make-card :clubs 5)]
                :damage 10
                :attack-reduce 0
                :hearts-heal 0
                :diamonds-draw 0
                :enemy-defeated true
                :exact-kill false
                :immune-suit :hearts
                :played-suits #{:clubs}}
        defeated-card (card/make-card :hearts 11)
        next-enemy (enemy/make-enemy (card/make-card :spades 11))
        output (display/render-enemy-defeated action defeated-card next-enemy 11)]
    (is (string? output))
    (is (re-find #"ENEMY DEFEATED" output))
    (is (re-find #"Enemies remaining: 11" output))
    (is (re-find #"Jack of spades" output)
        "Should preview the next enemy")))

(deftest render-enemy-defeated-last-enemy-test
  (let [action {:played [(card/make-card :hearts 10)]
                :damage 10
                :attack-reduce 0
                :hearts-heal 0
                :diamonds-draw 0
                :enemy-defeated true
                :exact-kill true
                :immune-suit :diamonds
                :played-suits #{:hearts}}
        defeated-card (card/make-card :diamonds 13)
        output (display/render-enemy-defeated action defeated-card nil 0)]
    (is (string? output))
    (is (re-find #"ENEMY DEFEATED" output))
    (is (re-find #"Exact kill" output))
    (is (not (re-find #"Next enemy" output))
        "Should not show next enemy when none remain")))

(deftest render-game-over-test
  (is (re-find #"VICTORY" (display/render-game-over {:status :won})))
  (is (re-find #"DEFEAT" (display/render-game-over {:status :lost}))))

(deftest render-help-test
  (let [output (display/render-help)]
    (is (string? output))
    (is (re-find #"HELP" output))))

(deftest render-welcome-test
  (let [output (display/render-welcome)]
    (is (string? output))
    (is (re-find #"(?i)regicide" output))))

(deftest render-selector-prompt-test
  (is (re-find #"play" (display/render-selector-prompt :play-cards)))
  (is (re-find #"discard" (display/render-selector-prompt :suffer-damage))))
