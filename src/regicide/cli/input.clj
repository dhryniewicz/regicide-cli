(ns regicide.cli.input
  (:require [clojure.string :as str]))

(defn parse-command
  "Parse line input into a command map. Only handles card number selection
   since single-key commands (p/h/q) are handled at the raw input level."
  [input phase hand-size]
  (let [trimmed (str/trim (str input))]
    (if (str/blank? trimmed)
      {:type :invalid :message "Enter card numbers (e.g., '3' or '2 4' for a combo)."}
      (let [parts (str/split trimmed #"\s+")
            nums (try
                   (mapv #(Integer/parseInt %) parts)
                   (catch Exception _
                     nil))]
        (if (nil? nums)
          {:type :invalid :message (str "Could not parse '" trimmed "'. Enter card numbers.")}
          (let [indices (mapv dec nums)
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
