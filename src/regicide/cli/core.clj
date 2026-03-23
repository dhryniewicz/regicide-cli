(ns regicide.cli.core
  (:require [regicide.card :as card]
            [regicide.game :as game]
            [regicide.rules :as rules]
            [regicide.cli.display :as display]
            [regicide.cli.input :as input]))

(defn- read-input []
  (flush)
  (read-line))

(defn- show [& texts]
  (doseq [t texts]
    (when t (println t))))

(defn- play-phase
  "Handle the play-cards phase. Returns [new-state action-info] or nil on quit."
  [state]
  (let [hand (game/current-hand state)]
    (if (empty? hand)
      ;; No cards to play - auto-lose in solo
      [(game/check-can-play state) nil]
      (do
        (print (display/render-play-prompt))
        (let [input (read-input)]
          (when (nil? input) ;; EOF
            (System/exit 0))
          (let [cmd (input/parse-command input :play-cards (count hand))]
            (case (:type cmd)
              :quit nil
              :help (do (show (display/render-help)) :retry)
              :state (do (show (display/render-game-state state)) :retry)
              :invalid (do (show (str "  " (:message cmd))) :retry)
              :play
              (let [result (game/play-cards state (:indices cmd))]
                (if (:error result)
                  (do (show (str "  " (:error result))) :retry)
                  (let [cards (mapv (game/current-hand state) (:indices cmd))
                        effects (rules/suit-effects cards (:current-enemy state))
                        damage (rules/total-damage cards (:current-enemy state))
                        enemy (:current-enemy state)
                        defeated (>= damage (:health enemy))
                        exact (and defeated
                                   (= damage (:health enemy)))
                        action-info {:played cards
                                     :damage damage
                                     :attack-reduce (:attack-reduce effects)
                                     :hearts-heal (min (:hearts-heal effects)
                                                       (count (:discard-pile state)))
                                     :diamonds-draw (min (:diamonds-draw effects)
                                                         (count (:tavern-deck state)))
                                     :enemy-defeated defeated
                                     :exact-kill exact}]
                    [result action-info]))))))))))

(defn- discard-phase
  "Handle the suffer-damage phase. Returns [new-state action-info] or nil on quit."
  [state]
  (let [state (game/check-can-survive state)]
    (if (= :lost (:status state))
      [state nil]
      (let [hand (game/current-hand state)
            attack (get-in state [:current-enemy :attack])]
        (print (display/render-discard-prompt attack))
        (let [input (read-input)]
          (when (nil? input)
            (System/exit 0))
          (let [cmd (input/parse-command input :suffer-damage (count hand))]
            (case (:type cmd)
              :quit nil
              :help (do (show (display/render-help)) :retry)
              :state (do (show (display/render-game-state state)) :retry)
              :invalid (do (show (str "  " (:message cmd))) :retry)
              :discard
              (let [result (game/suffer-damage state (:indices cmd))]
                (if (:error result)
                  (do (show (str "  " (:error result))) :retry)
                  (let [cards (mapv hand (:indices cmd))
                        action-info {:discarded cards}]
                    [result action-info]))))))))))

(defn- game-loop [state]
  (loop [state state
         action-info nil]
    (when (= :in-progress (:status state))
      (show (display/render-game-state state))
      (when action-info
        (show (display/render-action-result action-info)))
      (let [result (case (:phase state)
                     :play-cards   (play-phase state)
                     :suffer-damage (discard-phase state)
                     :game-over    nil)]
        (cond
          (nil? result)
          (show "\nThanks for playing!")

          (= :retry result)
          (recur state nil)

          :else
          (let [[new-state new-action] result]
            (if (#{:won :lost} (:status new-state))
              (do
                (show (display/render-game-state new-state))
                (when new-action
                  (show (display/render-action-result new-action)))
                (show (display/render-game-over new-state)))
              (recur new-state new-action))))))))

(defn -main [& args]
  (let [num-players (if (first args)
                      (Integer/parseInt (first args))
                      1)]
    (when-not (<= 1 num-players 4)
      (println "Number of players must be 1-4.")
      (System/exit 1))
    (show (display/render-welcome))
    (show (str "Starting game with " num-players " player(s)..."))
    (let [state (game/new-game num-players)]
      (game-loop state))))
