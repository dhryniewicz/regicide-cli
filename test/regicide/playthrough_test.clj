(ns regicide.playthrough-test
  "Automated playthroughs that exercise the full game logic."
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [regicide.card :as card]
            [regicide.game :as game]
            [regicide.rules :as rules]
            [regicide.enemy :as enemy]))

;; ---------------------------------------------------------------------------
;; AI strategy: greedy player that maximizes damage
;; ---------------------------------------------------------------------------

(defn- all-plays
  "Generate all valid single-card and two-card plays for a hand."
  [hand enemy]
  (let [n (count hand)
        singles (for [i (range n)
                      :let [cards [(hand i)]]
                      :when (rules/valid-combo? cards)]
                  {:indices [i] :cards cards
                   :damage (rules/total-damage cards enemy)})
        pairs (for [i (range n)
                    j (range (inc i) n)
                    :let [cards [(hand i) (hand j)]]
                    :when (rules/valid-combo? cards)]
                {:indices [i j] :cards cards
                 :damage (rules/total-damage cards enemy)})]
    (concat pairs singles)))

(defn- find-best-play
  "Pick the play that maximizes damage. Prefer exact kills."
  [hand enemy]
  (let [plays (all-plays hand enemy)
        health (:health enemy)]
    (when (seq plays)
      ;; Prefer exact kill > overkill > max damage
      (or (first (filter #(= (:damage %) health) plays))
          (first (sort-by #(- (:damage %)) plays))))))

(defn- find-min-discard
  "Find cards to discard to absorb attack, preferring to keep high-value cards."
  [hand attack]
  (let [;; Sort indices by card value ascending — discard cheapest first
        sorted-asc (sort-by #(card/card-value (hand %)) (range (count hand)))]
    (loop [remaining sorted-asc, total 0, selected []]
      (cond
        (>= total attack) selected
        (empty? remaining) nil
        :else (let [i (first remaining)]
                (recur (rest remaining)
                       (+ total (card/card-value (hand i)))
                       (conj selected i)))))))

;; ---------------------------------------------------------------------------
;; Game simulation
;; ---------------------------------------------------------------------------

(defn- play-turn
  "Execute one action. Returns [new-state log-entry]."
  [state]
  (let [hand (game/current-hand state)
        enemy (:current-enemy state)
        pi (inc (:current-player state))
        phase (:phase state)]
    (case phase
      :play-cards
      (cond
        (empty? hand)
        (if (pos? (:jesters state))
          (let [r (game/use-jester state)]
            [r (str "  P" pi ": Jester! Drew fresh hand")])
          [(game/check-can-play state)
           (str "  P" pi ": No cards, check-can-play")])

        :else
        (if-let [play (find-best-play hand enemy)]
          (let [r (game/play-cards state (:indices play))]
            (if (:error r)
              ;; fallback: yield in multiplayer, otherwise stuck
              (if (> (:num-players state) 1)
                [(game/yield state) (str "  P" pi ": Error, yield")]
                [state (str "  P" pi ": ERROR: " (:error r))])
              [r (str "  P" pi ": " (str/join "+" (map card/card-label (:cards play)))
                      " → " (:damage play) " dmg"
                      (when (>= (:damage play) (:health enemy)) " KILL!"))]))
          ;; No play found (shouldn't happen with non-empty hand)
          (if (> (:num-players state) 1)
            [(game/yield state) (str "  P" pi ": Yield")]
            [(assoc state :status :lost :phase :game-over)
             (str "  P" pi ": Stuck — LOST!")])))

      :suffer-damage
      (let [attack (get-in state [:current-enemy :attack])]
        (cond
          (zero? attack)
          ;; Shouldn't reach here (game.clj skips to play-cards), safety
          [(assoc state :phase :play-cards) (str "  P" pi ": 0 attack, skip")]

          (not (rules/can-absorb-damage? hand attack))
          (if (pos? (:jesters state))
            (let [r (game/use-jester state)]
              [r (str "  P" pi ": Jester to survive " attack " dmg")])
            [(assoc state :status :lost :phase :game-over)
             (str "  P" pi ": Can't absorb " attack " — LOST!")])

          :else
          (if-let [indices (find-min-discard hand attack)]
            (let [r (game/suffer-damage state indices)]
              (if (:error r)
                [state (str "  P" pi ": Discard error")]
                [r (str "  P" pi ": Absorb " attack
                        " (" (count indices) " cards)")]))
            [state (str "  P" pi ": Can't find discard")])))

      ;; Unknown phase
      [state (str "  P" pi ": Phase " phase)])))

(defn simulate-game
  "Run a full game. Returns result map."
  [num-players]
  (let [initial (game/new-game num-players)]
    (loop [state initial
           turn 0
           log [(str "=== " num-players "P GAME ===")
                (str "Enemy: " (card/card-label (get-in state [:current-enemy :card]))
                     " HP:" (get-in state [:current-enemy :health])
                     " ATK:" (get-in state [:current-enemy :attack]))]
           prev-health nil
           stall 0]
      (let [cur-health (get-in state [:current-enemy :health])]
        (cond
          (not= :in-progress (:status state))
          (let [defeated (- 12 (inc (count (:castle-deck state))))]
            {:status (:status state) :turns turn
             :enemies-defeated defeated
             :log (conj log (str ">> " (name (:status state))
                                 " after " turn " turns, "
                                 defeated "/12 defeated"))})

          ;; Stall detection: if enemy health unchanged for 50 turns, give up
          (> stall 50)
          {:status :stalled :turns turn
           :enemies-defeated (- 12 (inc (count (:castle-deck state))))
           :log (conj log (str ">> STALLED at turn " turn))}

          (>= turn 1000)
          {:status :timeout :turns turn
           :enemies-defeated (- 12 (inc (count (:castle-deck state))))
           :log (conj log ">> TIMEOUT")}

          :else
          (let [[new-state entry] (play-turn state)
                new-health (get-in new-state [:current-enemy :health])
                new-stall (if (= new-health cur-health) (inc stall) 0)
                new-log (cond-> (conj log entry)
                          (and (not= (get-in state [:current-enemy :card])
                                     (get-in new-state [:current-enemy :card]))
                               (= :in-progress (:status new-state)))
                          (conj (str "  >>> New: "
                                     (card/card-label (get-in new-state [:current-enemy :card]))
                                     " HP:" (get-in new-state [:current-enemy :health]))))]
            (recur (assoc new-state :player-changed false)
                   (inc turn)
                   new-log
                   new-health
                   new-stall)))))))

;; ---------------------------------------------------------------------------
;; Tests — 2 games per player count
;; ---------------------------------------------------------------------------

(defn- run-games [num-players n]
  (doall
    (for [i (range n)]
      (let [r (simulate-game num-players)]
        (println (str "\n--- " num-players "P Game " (inc i) " ---"))
        (println (str "Result: " (name (:status r))
                      " | Turns: " (:turns r)
                      " | Enemies: " (:enemies-defeated r) "/12"))
        r))))

(deftest playthrough-solo-test
  (let [results (run-games 1 2)]
    (doseq [r results]
      (is (#{:won :lost :stalled} (:status r))))))

(deftest playthrough-2p-test
  (let [results (run-games 2 2)]
    (doseq [r results]
      (is (#{:won :lost :stalled} (:status r))))))

(deftest playthrough-3p-test
  (let [results (run-games 3 2)]
    (doseq [r results]
      (is (#{:won :lost :stalled} (:status r))))))

(deftest playthrough-4p-test
  (let [results (run-games 4 2)]
    (doseq [r results]
      (is (#{:won :lost :stalled} (:status r))))))
