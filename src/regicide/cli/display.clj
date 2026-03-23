(ns regicide.cli.display
  (:require [regicide.card :as card]
            [regicide.game :as game]
            [regicide.cli.terminal :as term]
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

(def ^:private sort-order-labels
  {:none "unsorted" :suit "by suit" :rank "by rank"})

(defn sorted-hand-with-indices
  "Returns a vec of [original-index card] pairs, sorted according to sort-order."
  [hand sort-order]
  (let [indexed (map-indexed vector hand)]
    (vec
      (case sort-order
        :suit (sort-by (fn [[_ c]] [(card/suit-order (:suit c)) (:rank c)]) indexed)
        :rank (sort-by (fn [[_ c]] [(:rank c) (card/suit-order (:suit c))]) indexed)
        indexed))))

(defn render-hand-selector
  "Render the hand with cursor and selection indicators.
   cursor-pos is position in the display order (0-based).
   selected is a set of original indices that are selected."
  [hand sort-order cursor-pos selected]
  (if (empty? hand)
    "  (empty)"
    (let [display-cards (sorted-hand-with-indices hand sort-order)]
      (str/join "  "
        (map-indexed
          (fn [display-idx [orig-idx c]]
            (let [is-cursor (= display-idx cursor-pos)
                  is-selected (contains? selected orig-idx)
                  label (card/card-label c)]
              (cond
                (and is-cursor is-selected)
                (str term/reverse-video term/underline "\u2713" label term/reset)

                is-cursor
                (str term/reverse-video " " label term/reset)

                is-selected
                (str term/underline "\u2713" label term/reset)

                :else
                (str " " label))))
          display-cards)))))

(defn render-hand-static
  "Render hand without selection UI (for game state display without interaction)."
  [hand sort-order]
  (if (empty? hand)
    "  (empty)"
    (str/join "  "
      (map (fn [[_ c]] (str " " (card/card-label c)))
           (sorted-hand-with-indices hand sort-order)))))

(def ^:private yellow "\u001b[33m")
(def ^:private red "\u001b[31m")
(def ^:private ansi-reset "\u001b[0m")

(defn- render-phase-banner [phase enemy]
  (let [attack (:attack enemy)]
    (case phase
      :play-cards
      (str term/bold ">>> ATTACK " ansi-reset "- Select cards to play against the enemy")
      :suffer-damage
      (str term/bold red ">>> DEFEND " ansi-reset red
           "- Discard cards worth at least " attack " to survive" ansi-reset)
      "")))

(defn render-game-state
  "Render the full game state. When selector-state is provided, shows interactive hand."
  ([state] (render-game-state state nil))
  ([state selector-state]
   (let [{:keys [tavern-deck discard-pile castle-deck current-enemy sort-order phase]} state
         sort-order (or sort-order :none)
         hand (game/current-hand state)
         enemies-remaining (inc (count castle-deck))
         hand-display (if selector-state
                        (render-hand-selector hand sort-order
                                             (:cursor selector-state)
                                             (:selected selector-state))
                        (render-hand-static hand sort-order))]
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
        (render-phase-banner phase current-enemy)
        ""
        (str "Your Hand (" (sort-order-labels sort-order) "):")
        (str "  " hand-display)
        ""]))))

(defn render-selector-prompt [phase]
  (case phase
    :play-cards    "\u2190\u2192 move  \u2191 select  Enter play  p sort  h help  q quit"
    :suffer-damage "\u2190\u2192 move  \u2191 select  Enter discard  p sort  h help  q quit"))

(defn render-help []
  (str/join "\n"
    [""
     "=== HELP ==="
     ""
     "  Controls:"
     "    \u2190 \u2192     - Move cursor left/right"
     "    \u2191        - Toggle card selection"
     "    Enter    - Play/discard selected cards"
     "    p        - Toggle hand sorting (unsorted -> by suit -> by rank)"
     "    h        - Show this help"
     "    q        - Quit the game"
     ""
     "  Suit Powers:"
     (str "    " (card/suit-symbols :spades) " Spades   - Reduce enemy attack")
     (str "    " (card/suit-symbols :hearts) " Hearts   - Shuffle discard into tavern")
     (str "    " (card/suit-symbols :diamonds) " Diamonds - Draw cards")
     (str "    " (card/suit-symbols :clubs) " Clubs    - Double damage")
     ""
     "  Combos: Play multiple cards of the same rank, or Ace + card(s)"
     "  Exact kill: If damage equals remaining health exactly, enemy goes to your hand"
     ""
     "  Press any key to continue..."]))

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

(defn render-action-result [action-info]
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
