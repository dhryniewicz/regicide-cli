(ns regicide.cli.core
  (:require [regicide.card :as card]
            [regicide.game :as game]
            [regicide.rules :as rules]
            [regicide.cli.display :as display]
            [regicide.cli.input :as input]
            [regicide.cli.terminal :as term]))

(def ^:private sort-cycle {:none :suit, :suit :rank, :rank :none})

(defn- toggle-sort [state]
  (update state :sort-order #(sort-cycle (or % :none))))

(defn- show [& texts]
  (doseq [t texts]
    (when t (println t))))

(defn- read-input
  "Read input in raw mode. Single-key commands (p/h/q) fire instantly.
   Digit input accumulates until Enter for card selection."
  [prompt]
  (print prompt)
  (flush)
  (loop []
    (let [c (term/read-char)]
      (cond
        (nil? c) nil

        ;; Instant single-key commands
        (= c \p) {:type :sort}
        (= c \q) {:type :quit}
        (or (= c \h) (= c \?)) {:type :help}

        ;; Digit starts a card selection - echo it and read rest of line
        (Character/isDigit c)
        (do (print c) (flush)
            (let [rest-line (term/read-line-raw)]
              (when rest-line
                {:type :line :text (str c rest-line)})))

        ;; Enter on empty input
        (or (= c \newline) (= c \return))
        (do (println) {:type :line :text ""})

        ;; Ignore other keys
        :else (recur)))))

(defn- process-input
  "Read input and return a command map for the given phase."
  [prompt phase hand-size]
  (let [result (read-input prompt)]
    (cond
      (nil? result) nil
      (#{:sort :quit :help} (:type result)) result
      (= :line (:type result))
      (input/parse-command (:text result) phase hand-size))))

(defn- play-phase
  "Handle the play-cards phase. Returns [new-state action-info] or nil on quit."
  [state]
  (let [hand (game/current-hand state)]
    (if (empty? hand)
      [(game/check-can-play state) nil]
      (let [cmd (process-input (display/render-play-prompt) :play-cards (count hand))]
        (case (:type cmd)
          nil nil
          :quit nil
          :help (do (println) (show (display/render-help)) :retry)
          :sort [(toggle-sort state) nil]
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
                [result action-info]))))))))

(defn- discard-phase
  "Handle the suffer-damage phase. Returns [new-state action-info] or nil on quit."
  [state]
  (let [state (game/check-can-survive state)]
    (if (= :lost (:status state))
      [state nil]
      (let [hand (game/current-hand state)
            attack (get-in state [:current-enemy :attack])]
        (let [cmd (process-input (display/render-discard-prompt attack) :suffer-damage (count hand))]
          (case (:type cmd)
            nil nil
            :quit nil
            :help (do (println) (show (display/render-help)) :retry)
            :sort [(toggle-sort state) nil]
            :invalid (do (show (str "  " (:message cmd))) :retry)
            :discard
            (let [result (game/suffer-damage state (:indices cmd))]
              (if (:error result)
                (do (show (str "  " (:error result))) :retry)
                (let [cards (mapv hand (:indices cmd))
                      action-info {:discarded cards}]
                  [result action-info])))))))))

(defn- game-loop [state]
  (loop [state state
         action-info nil]
    (when (= :in-progress (:status state))
      (show (display/render-game-state state))
      (when action-info
        (show (display/render-action-result action-info)))
      (let [result (case (:phase state)
                     :play-cards    (play-phase state)
                     :suffer-damage (discard-phase state)
                     :game-over     nil)]
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
    (term/enable-raw-mode!)
    (try
      (show (display/render-welcome))
      (show (str "Starting game with " num-players " player(s)..."))
      (let [state (game/new-game num-players)]
        (game-loop state))
      (finally
        (term/disable-raw-mode!)))))
