(ns regicide.cli.display
  (:require [regicide.card :as card]
            [regicide.game :as game]
            [clojure.string :as str]))

(defn health-bar [current max-val width]
  (let [filled (if (pos? max-val)
                 (int (* width (/ (max 0 current) max-val)))
                 0)
        empty (- width filled)]
    (str (apply str (repeat filled "\u2588"))
         (apply str (repeat empty "\u2591")))))

(defn render-enemy [enemy]
  (let [{:keys [card health max-health attack max-attack]} enemy
        label (card/card-label card)
        name (case (:rank card)
               11 "Jack"
               12 "Queen"
               13 "King")
        immune-suit (card/suit-symbols (:suit card))]
    (str/join "\n"
      [(str "  " name " of " (clojure.core/name (:suit card)) " [" label "]")
       (str "  Health: " health "/" max-health "  " (health-bar health max-health 20))
       (str "  Attack: " attack "/" max-attack
            (when (< attack max-attack)
              (str "  (reduced by " (- max-attack attack) ")")))
       (str "  Immune to: " immune-suit " (" (clojure.core/name (:suit card)) ")")])))

(defn render-hand [hand]
  (if (empty? hand)
    "  (empty)"
    (str/join "  "
      (map-indexed (fn [i c]
                     (str "[" (inc i) "] " (card/card-label c)))
                   hand))))

(defn render-game-state [state]
  (let [{:keys [tavern-deck discard-pile castle-deck current-enemy]} state
        hand (game/current-hand state)
        enemies-remaining (inc (count castle-deck))]
    (str/join "\n"
      [""
       "=== REGICIDE ==="
       ""
       "Current Enemy:"
       (render-enemy current-enemy)
       ""
       (str "  Tavern: " (count tavern-deck) " cards"
            "  |  Discard: " (count discard-pile) " cards"
            "  |  Enemies remaining: " enemies-remaining)
       ""
       "Your Hand:"
       (str "  " (render-hand hand))
       ""])))

(defn render-play-prompt []
  "Play cards (numbers separated by spaces), or type 'help': ")

(defn render-discard-prompt [attack]
  (str "Discard cards to absorb " attack " damage (numbers separated by spaces): "))

(defn render-help []
  (str/join "\n"
    [""
     "=== HELP ==="
     "  Play phase:   Enter card numbers to play (e.g., '3' or '2 4' for a combo)"
     "  Discard phase: Enter card numbers to discard to absorb enemy attack"
     "  Commands:"
     "    h, help, ?  - Show this help"
     "    q, quit     - Quit the game"
     ""
     "  Suit Powers:"
     (str "    " (card/suit-symbols :spades) " Spades   - Reduce enemy attack")
     (str "    " (card/suit-symbols :hearts) " Hearts   - Shuffle discard into tavern")
     (str "    " (card/suit-symbols :diamonds) " Diamonds - Draw cards")
     (str "    " (card/suit-symbols :clubs) " Clubs    - Double damage")
     ""
     "  Combos: Play multiple cards of the same rank, or Ace + card(s)"
     "  Exact kill: If damage equals remaining health exactly, enemy goes to your hand"
     ""]))

(defn render-game-over [state]
  (case (:status state)
    :won  "\n*** VICTORY! You have defeated all enemies! ***\n"
    :lost "\n*** DEFEAT! You could not survive the enemy's attack. ***\n"
    ""))

(defn render-welcome []
  (str/join "\n"
    [""
     "  ____            _      _     _      "
     " |  _ \\ ___  __ _(_) ___(_) __| | ___ "
     " | |_) / _ \\/ _` | |/ __| |/ _` |/ _ \\"
     " |  _ \\  __/ (_| | | (__| | (_| |  __/"
     " |_| \\_\\___|\\__, |_|\\___|_|\\__,_|\\___|"
     "            |___/                      "
     ""
     "  A cooperative card game of regicide"
     ""]))

(defn render-action-result
  "Render a summary of what just happened."
  [action-info]
  (when action-info
    (str/join "\n"
      (remove nil?
        [(when-let [cards (:played action-info)]
           (str "  Played: " (str/join " " (map card/card-label cards))))
         (when-let [dmg (:damage action-info)]
           (str "  Damage dealt: " dmg))
         (when (pos? (or (:attack-reduce action-info) 0))
           (str "  " (card/suit-symbols :spades) " Reduced enemy attack by " (:attack-reduce action-info)))
         (when (pos? (or (:hearts-heal action-info) 0))
           (str "  " (card/suit-symbols :hearts) " Recycled " (:hearts-heal action-info) " cards from discard"))
         (when (pos? (or (:diamonds-draw action-info) 0))
           (str "  " (card/suit-symbols :diamonds) " Drew " (:diamonds-draw action-info) " cards"))
         (when (:enemy-defeated action-info)
           (if (:exact-kill action-info)
             "  >> Enemy defeated! (Exact kill - enemy card added to your hand!)"
             "  >> Enemy defeated!"))
         (when-let [discarded (:discarded action-info)]
           (str "  Discarded to absorb damage: " (str/join " " (map card/card-label discarded))))]))))
