(ns regicide.cli.core
  (:require [regicide.game :as game]
            [regicide.rules :as rules]
            [regicide.cli.display :as display]
            [regicide.cli.terminal :as term]))

(def ^:private sort-cycle {:none :suit, :suit :rank, :rank :none})

(defn- toggle-sort [state]
  (update state :sort-order #(sort-cycle (or % :none))))

(defn- show [& texts]
  (doseq [t texts]
    (when t (println t))))

(defn- multiplayer? [state]
  (> (:num-players state) 1))

(defn- confirm-quit? []
  (println "\n  Quit game? (y/n)")
  (flush)
  (loop []
    (let [key (term/read-key)]
      (cond
        (or (= key \y) (= key \Y)) true
        (or (= key \n) (= key \N) (= key :escape)) false
        :else (recur)))))

(defn- show-error! [state msg]
  (print term/clear-screen)
  (show (display/render-game-state state))
  (show (str "\n  " msg))
  (println "\n  Press any key...")
  (flush)
  (term/read-key))

(defn- redraw! [state selector-state action-info phase]
  (print term/clear-screen)
  (show (display/render-game-state state selector-state))
  (when action-info
    (show (display/render-action-result action-info)))
  (println (display/render-selector-prompt phase (multiplayer? state)))
  (flush))

(defn- hot-seat-transition!
  "Show transition screen between players if needed."
  [state]
  (if (and (multiplayer? state) (:player-changed state))
    (do (print term/clear-screen)
        (show (display/render-turn-transition (inc (:current-player state))))
        (flush)
        (term/read-key)
        (assoc state :player-changed false))
    (assoc state :player-changed false)))

(defn- player-chooser-loop!
  "Let the current player choose who goes next (for multiplayer jester)."
  [state]
  (print term/clear-screen)
  (show (display/render-game-state state))
  (show (display/render-player-chooser state))
  (flush)
  (loop []
    (let [key (term/read-key)]
      (if (and (char? key) (Character/isDigit ^char key))
        (let [idx (dec (Character/digit ^char key 10))]
          (if (and (>= idx 0)
                   (< idx (:num-players state))
                   (not= idx (:current-player state)))
            (game/choose-next-player state idx)
            (recur)))
        (recur)))))

(defn- card-select-loop
  "Interactive card selection with arrow keys.
   Returns selected original indices as a vec, or a keyword command."
  [state phase action-info]
  (let [hand (game/current-hand state)
        hand-size (count hand)]
    (when (pos? hand-size)
      (loop [cursor 0
             selected #{}]
        (redraw! state {:cursor cursor :selected selected} action-info phase)
        (let [key (term/read-key)]
          (case key
            :left  (recur (mod (dec cursor) hand-size) selected)
            :right (recur (mod (inc cursor) hand-size) selected)
            :up    (let [display-cards (display/sorted-hand-with-indices hand (or (:sort-order state) :none))
                         [orig-idx _] (nth display-cards cursor)
                         new-selected (if (contains? selected orig-idx)
                                        (disj selected orig-idx)
                                        (conj selected orig-idx))]
                     (recur cursor new-selected))
            :enter (if (empty? selected)
                     (recur cursor selected)
                     (vec selected))
            :quit  (recur cursor selected)
            (:escape :down) (recur cursor selected)
            (cond
              (= key \p) :sort
              (= key \j) :jester
              (and (= key \y) (= phase :play-cards) (multiplayer? state)) :yield
              (= key \q) (if (confirm-quit?) :quit (recur cursor selected))
              (or (= key \h) (= key \?)) :help
              :else (recur cursor selected))))))))

(defn- handle-jester [state action-info]
  (if (multiplayer? state)
    ;; Multiplayer: cancel immunity, skip attack, choose next player
    (let [result (game/play-jester state)]
      (if (:error result)
        (do (show-error! state (:error result)) [state action-info])
        (let [next-state (player-chooser-loop! result)]
          (if (:error next-state)
            (do (show-error! state (:error next-state)) [state action-info])
            [next-state {:jester-used true}]))))
    ;; Solo: refresh hand
    (let [result (game/use-jester state)]
      (if (:error result)
        (do (show-error! state (:error result)) [state nil])
        [result {:jester-used true}]))))

(defn- play-phase
  "Handle the play-cards phase. Returns [new-state action-info] or nil on quit."
  [state action-info]
  (let [hand (game/current-hand state)]
    (if (empty? hand)
      (if (pos? (:jesters state))
        ;; Empty hand but jester available — prompt to use it
        (do (print term/clear-screen)
            (show (display/render-game-state state))
            (show (str "\n  No cards in hand! Press j to use a jester, or q to quit."))
            (flush)
            (loop []
              (let [key (term/read-key)]
                (cond
                  (= key \j) (handle-jester state action-info)
                  (= key \q) (if (confirm-quit?) nil (recur))
                  :else (recur)))))
        ;; No jesters either — check for loss or auto-yield
        (let [checked (game/check-can-play state)]
          (when (:auto-yielded checked)
            (print term/clear-screen)
            (show (str "\n  Player " (inc (:current-player state))
                       " has no cards — auto-yielding."))
            (println "\n  Press any key...")
            (flush)
            (term/read-key))
          [checked nil]))
      (let [result (card-select-loop state :play-cards action-info)]
        (cond
          (nil? result) nil
          (= :quit result) nil
          (= :sort result) [(toggle-sort state) action-info]
          (= :yield result)
          (let [yield-result (game/yield state)]
            (if (:error yield-result)
              (do (show-error! state (:error yield-result)) [state action-info])
              [yield-result nil]))
          (= :jester result) (handle-jester state action-info)
          (= :help result)
          (do (print term/clear-screen)
              (show (display/render-help))
              (term/read-key)
              [state action-info])

          :else
          (let [play-result (game/play-cards state result)]
            (if (:error play-result)
              (do (show-error! state (:error play-result)) [state nil])
              (let [cards (mapv (game/current-hand state) result)
                    effects (rules/suit-effects cards (:current-enemy state))
                    damage (rules/total-damage cards (:current-enemy state))
                    enemy (:current-enemy state)
                    defeated (>= damage (:health enemy))
                    exact (and defeated (= damage (:health enemy)))
                    immune-suit (get-in state [:current-enemy :card :suit])
                    played-suits (set (map :suit cards))
                    new-action {:played cards
                                :damage damage
                                :attack-reduce (:attack-reduce effects)
                                :hearts-heal (min (:hearts-heal effects)
                                                  (count (:discard-pile state)))
                                :diamonds-draw (min (:diamonds-draw effects)
                                                    (count (:tavern-deck state)))
                                :enemy-defeated defeated
                                :exact-kill exact
                                :immune-suit immune-suit
                                :played-suits played-suits}]
                [play-result new-action]))))))))

(defn- discard-phase
  "Handle the suffer-damage phase. Returns [new-state action-info] or nil on quit."
  [state action-info]
  (let [state (game/check-can-survive state)]
    (cond
      (= :lost (:status state))
      [state nil]

      ;; Can't absorb but have a jester — prompt to use it
      (and (not (rules/can-absorb-damage? (game/current-hand state)
                                           (get-in state [:current-enemy :attack])))
           (pos? (:jesters state)))
      (do (print term/clear-screen)
          (show (display/render-game-state state))
          (show (str "\n  Not enough cards to survive! Press j to use a jester, or q to quit."))
          (flush)
          (loop []
            (let [key (term/read-key)]
              (cond
                (= key \j) (handle-jester state action-info)
                (= key \q) (if (confirm-quit?) nil (recur))
                :else (recur)))))

      :else
      (let [result (card-select-loop state :suffer-damage action-info)]
        (cond
          (nil? result) nil
          (= :quit result) nil
          (= :sort result) [(toggle-sort state) action-info]
          (= :jester result)
          (if (multiplayer? state)
            ;; Multiplayer jester only during play phase
            (do (show-error! state "Jesters can only be played during the attack phase.")
                [state action-info])
            (handle-jester state action-info))
          (= :help result)
          (do (print term/clear-screen)
              (show (display/render-help))
              (term/read-key)
              [state action-info])

          :else
          (let [discard-result (game/suffer-damage state result)]
            (if (:error discard-result)
              (do (show-error! state (:error discard-result)) [state action-info])
              (let [hand (game/current-hand state)
                    cards (mapv hand result)
                    new-action {:discarded cards}]
                [discard-result new-action]))))))))

(defn- game-loop [state]
  (loop [state state
         action-info nil]
    (when (= :in-progress (:status state))
      ;; Hot-seat transition between players
      (let [state (hot-seat-transition! state)]
        (let [[new-state new-action]
              (case (:phase state)
                :play-cards    (play-phase state action-info)
                :suffer-damage (discard-phase state action-info)
                :game-over     nil)]
          (cond
            (nil? new-state)
            (do (print term/clear-screen) (flush)
                (show "\nThanks for playing!"))

            (#{:won :lost} (:status new-state))
            (do (print term/clear-screen)
                (show (display/render-game-state new-state))
                (when new-action
                  (show (display/render-action-result new-action)))
                (show (display/render-game-over new-state)))

            :else
            (do
              (when (:enemy-defeated new-action)
                (let [enemies-left (inc (count (:castle-deck new-state)))
                      next-enemy (:current-enemy new-state)]
                  (print term/clear-screen)
                  (show (display/render-enemy-defeated new-action
                          (get-in state [:current-enemy :card])
                          next-enemy enemies-left))
                  (flush)
                  (term/read-key)))
              (recur new-state nil))))))))

(defn -main [& args]
  (let [num-players (if (first args)
                      (Integer/parseInt (first args))
                      1)]
    (when-not (<= 1 num-players 4)
      (println "Number of players must be 1-4.")
      (System/exit 1))
    (term/enable-raw-mode!)
    (try
      (show (display/render-welcome))
      (show (str "Starting game with " num-players " player(s)..."))
      (println "\nPress any key to start...")
      (flush)
      (term/read-key)
      (let [state (game/new-game num-players)]
        (game-loop state))
      (finally
        (term/disable-raw-mode!)))))
