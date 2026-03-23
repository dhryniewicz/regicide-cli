(ns regicide.cli.input
  (:require [clojure.string :as str]))

(defn parse-command
  "Parse player input into a command map.
   Returns {:type :play/:discard/:yield/:quit/:help/:state/:invalid, ...}"
  [input phase hand-size]
  (let [trimmed (str/trim (str/lower-case (str input)))]
    (cond
      (contains? #{"q" "quit" "exit"} trimmed)
      {:type :quit}

      (contains? #{"h" "help" "?"} trimmed)
      {:type :help}

      (contains? #{"s" "state"} trimmed)
      {:type :state}

      (str/blank? trimmed)
      {:type :invalid :message "Please enter card numbers or a command."}

      :else
      (let [parts (str/split trimmed #"\s+")
            nums (try
                   (mapv #(Integer/parseInt %) parts)
                   (catch Exception _
                     nil))]
        (if (nil? nums)
          {:type :invalid :message (str "Could not parse '" trimmed "'. Enter card numbers (e.g., '1 3') or 'help'.")}
          (let [indices (mapv dec nums) ;; 1-based input to 0-based
                out-of-range (some #(or (neg? %) (>= % hand-size)) indices)]
            (cond
              out-of-range
              {:type :invalid
               :message (str "Card numbers must be between 1 and " hand-size ".")}

              (not= (count (set indices)) (count indices))
              {:type :invalid
               :message "Duplicate card numbers are not allowed."}

              :else
              {:type (if (= phase :suffer-damage) :discard :play)
               :indices indices})))))))
