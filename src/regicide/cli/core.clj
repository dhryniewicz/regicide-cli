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

(defn- redraw!
  "Clear screen and redraw the game state with selector."
  [state selector-state action-info phase]
  (print term/clear-screen)
  (show (display/render-game-state state selector-state))
  (when action-info
    (show (display/render-action-result action-info)))
  (println (display/render-selector-prompt phase))
  (flush))

(defn- card-select-loop
  "Interactive card selection with arrow keys.
   Returns selected original indices as a vec, or :quit, or :help."
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
              (= key \q) :quit
              (or (= key \h) (= key \?)) :help
              :else (recur cursor selected))))))))

(defn- play-phase
  "Handle the play-cards phase. Returns [new-state action-info] or nil on quit."
  [state action-info]
  (let [hand (game/current-hand state)]
    (if (empty? hand)
      [(game/check-can-play state) nil]
      (let [result (card-select-loop state :play-cards action-info)]
        (cond
          (nil? result) nil
          (= :quit result) nil
          (= :sort result) [(toggle-sort state) action-info]
          (= :help result)
          (do (print term/clear-screen)
              (show (display/render-help))
              (term/read-key)
              [state action-info])

          :else ;; vec of indices
          (let [play-result (game/play-cards state result)]
            (if (:error play-result)
              (do (print term/clear-screen)
                  (show (display/render-game-state state))
                  (show (str "\n  " (:error play-result)))
                  (println "\n  Press any key...")
                  (flush)
                  (term/read-key)
                  [state nil])
              (let [cards (mapv (game/current-hand state) result)
                    effects (rules/suit-effects cards (:current-enemy state))
                    damage (rules/total-damage cards (:current-enemy state))
                    enemy (:current-enemy state)
                    defeated (>= damage (:health enemy))
                    exact (and defeated (= damage (:health enemy)))
                    new-action {:played cards
                                :damage damage
                                :attack-reduce (:attack-reduce effects)
                                :hearts-heal (min (:hearts-heal effects)
                                                  (count (:discard-pile state)))
                                :diamonds-draw (min (:diamonds-draw effects)
                                                    (count (:tavern-deck state)))
                                :enemy-defeated defeated
                                :exact-kill exact}]
                [play-result new-action]))))))))

(defn- discard-phase
  "Handle the suffer-damage phase. Returns [new-state action-info] or nil on quit."
  [state action-info]
  (let [state (game/check-can-survive state)]
    (if (= :lost (:status state))
      [state nil]
      (let [result (card-select-loop state :suffer-damage action-info)]
        (cond
          (nil? result) nil
          (= :quit result) nil
          (= :sort result) [(toggle-sort state) action-info]
          (= :help result)
          (do (print term/clear-screen)
              (show (display/render-help))
              (term/read-key)
              [state action-info])

          :else
          (let [discard-result (game/suffer-damage state result)]
            (if (:error discard-result)
              (do (print term/clear-screen)
                  (show (display/render-game-state state))
                  (show (str "\n  " (:error discard-result)))
                  (println "\n  Press any key...")
                  (flush)
                  (term/read-key)
                  [state action-info])
              (let [hand (game/current-hand state)
                    cards (mapv hand result)
                    new-action {:discarded cards}]
                [discard-result new-action]))))))))

(defn- game-loop [state]
  (loop [state state
         action-info nil]
    (when (= :in-progress (:status state))
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
            (recur new-state nil)))))))

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
