(ns regicide.cli.terminal
  (:require [clojure.string :as str]))

(defn- stty [& args]
  (let [cmd (into-array String ["/bin/sh" "-c" (str "stty " (str/join " " args) " < /dev/tty")])]
    (-> (Runtime/getRuntime) (.exec ^"[Ljava.lang.String;" cmd) (.waitFor))))

(defn enable-raw-mode! []
  (stty "-icanon" "-echo" "min" "1"))

(defn disable-raw-mode! []
  (stty "sane"))

(defn read-char []
  (let [b (.read System/in)]
    (when (>= b 0)
      (char b))))

(defn read-key
  "Read a keypress. Returns a keyword for special keys, or a char for regular ones.
   Special keys: :left :right :up :down :enter :backspace :escape"
  []
  (let [c (read-char)]
    (when c
      (case (int c)
        27 (if (pos? (.available System/in))
             (let [c2 (read-char)]
               (if (= c2 \[)
                 (let [c3 (read-char)]
                   (case c3
                     \A :up
                     \B :down
                     \C :right
                     \D :left
                     :escape))
                 :escape))
             :escape)
        10 :enter
        13 :enter
        127 :backspace
        c))))

;; ANSI escape helpers
(def clear-screen "\u001b[2J\u001b[H")
(def reverse-video "\u001b[7m")
(def bold "\u001b[1m")
(def underline "\u001b[4m")
(def reset "\u001b[0m")
