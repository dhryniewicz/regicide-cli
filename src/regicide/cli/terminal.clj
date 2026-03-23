(ns regicide.cli.terminal
  (:require [clojure.string :as str]))

(defn- stty [& args]
  (let [cmd (into-array String (concat ["/bin/sh" "-c" (str "stty " (str/join " " args) " < /dev/tty")]))]
    (-> (Runtime/getRuntime) (.exec ^"[Ljava.lang.String;" cmd) (.waitFor))))

(defn enable-raw-mode!
  "Put terminal in raw mode (no line buffering, no echo)."
  []
  (stty "-icanon" "-echo" "min" "1"))

(defn disable-raw-mode!
  "Restore terminal to normal mode."
  []
  (stty "sane"))

(defn read-char
  "Read a single character from stdin. Returns the char, or nil on EOF."
  []
  (let [b (.read System/in)]
    (when (>= b 0)
      (char b))))

(defn read-line-raw
  "Read characters until Enter, echoing as we go. Handles backspace.
   Returns the accumulated string, or nil on EOF."
  []
  (loop [buf []]
    (let [c (read-char)]
      (cond
        (nil? c) nil
        (or (= c \newline) (= c \return))
        (do (print "\n") (flush)
            (apply str buf))
        (or (= c \backspace) (= (int c) 127))
        (if (seq buf)
          (do (print "\b \b") (flush)
              (recur (pop buf)))
          (recur buf))
        :else
        (do (print c) (flush)
            (recur (conj buf c)))))))
