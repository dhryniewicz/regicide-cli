(ns regicide.cli.display
  (:require [regicide.card :as card]
            [regicide.game :as game]
            [regicide.rules :as rules]
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
(def ^:private dim "\u001b[2m")
(def ^:private strikethrough "\u001b[9m")
(def ^:private ansi-reset "\u001b[0m")

(def ^:private suit-power-labels
  {:spades   "Reduce enemy attack"
   :hearts   "Recycle from discard"
   :diamonds "Draw cards"
   :clubs    "Double damage"})

(def ^:private green "\u001b[32m")
(def ^:private cyan "\u001b[36m")

(defn- render-selection-preview
  "Render a live preview of what the current card selection will do."
  [hand selected-indices enemy phase]
  (let [cards (mapv hand (sort selected-indices))]
    (case phase
      :play-cards
      (if-not (rules/valid-combo? cards)
        (str "  " red "\u2716 Invalid combo" ansi-reset)
        (let [effects (rules/suit-effects cards enemy)
              damage (rules/total-damage cards enemy)
              health (:health enemy)
              kills (>= damage health)
              exact (= damage health)
              immune-suit (get-in enemy [:card :suit])
              played-suits (set (map :suit cards))
              lines (remove nil?
                      [(str "  " cyan "Preview: " ansi-reset
                            (str/join " + " (map card/card-label cards))
                            " = " damage " damage"
                            (when (:clubs? effects) " (clubs 2x)"))
                       (when (pos? (:attack-reduce effects))
                         (str "    " (card/suit-symbols :spades)
                              " Enemy attack " (:attack enemy)
                              " \u2192 " (max 0 (- (:attack enemy) (:attack-reduce effects)))))
                       (when (pos? (:hearts-heal effects))
                         (str "    " (card/suit-symbols :hearts)
                              " Recycle up to " (:hearts-heal effects) " cards"))
                       (when (pos? (:diamonds-draw effects))
                         (str "    " (card/suit-symbols :diamonds)
                              " Draw up to " (:diamonds-draw effects) " cards"))
                       (when (contains? played-suits immune-suit)
                         (str "    " dim (card/suit-symbols immune-suit)
                              " " (suit-power-labels immune-suit)
                              " — blocked" ansi-reset))
                       (cond
                         exact (str "  " green term/bold
                                    "\u2192 EXACT KILL! (enemy goes to top of draw pile)" ansi-reset)
                         kills (str "  " green term/bold
                                    "\u2192 Defeats enemy (" damage "/" health ")" ansi-reset)
                         :else (str "  " dim
                                    "\u2192 Enemy health: " health
                                    " \u2192 " (- health damage) ansi-reset))])]
          (str/join "\n" lines)))

      :suffer-damage
      (let [total (reduce + (map card/card-value cards))
            attack (:attack enemy)
            enough (>= total attack)]
        (str "  " cyan "Preview: " ansi-reset
             "discard value = " total "/" attack
             (if enough
               (str "  " green "\u2713 enough" ansi-reset)
               (str "  " red "\u2716 need " (- attack total) " more" ansi-reset))))

      nil)))

(defn- render-other-players
  "Show hand counts for other players (not their cards)."
  [state]
  (let [current (:current-player state)
        parts (for [i (range (:num-players state))
                    :when (not= i current)]
                (str "Player " (inc i) ": "
                     (count (get-in state [:players i :hand])) " cards"))]
    (str "  " (str/join "  |  " parts) "\n")))

(defn render-turn-transition
  "Render the hot-seat transition screen between players."
  [player-num]
  (str/join "\n"
    [""
     ""
     (str "  " term/bold "=== PLAYER " player-num "'s TURN ===" term/reset)
     ""
     "  Press any key when ready..."]))

(defn render-player-chooser
  "Render a player picker for the multiplayer jester choose-next-player effect."
  [state]
  (let [current (:current-player state)
        lines (for [i (range (:num-players state))
                    :when (not= i current)]
                (str "  [" (inc i) "] Player " (inc i)
                     " (" (count (get-in state [:players i :hand])) " cards)"))]
    (str/join "\n"
      (concat
        [""
         (str "  " cyan "Choose who goes next:" ansi-reset)
         ""]
        lines
        [""]))))

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
                        (render-hand-static hand sort-order))
         immune-suit (get-in current-enemy [:card :suit])
         cursor-warning (when selector-state
                          (let [display-cards (sorted-hand-with-indices hand sort-order)
                                [_ card-at-cursor] (nth display-cards (:cursor selector-state) nil)]
                            (when (and card-at-cursor (= (:suit card-at-cursor) immune-suit))
                              (str "  " dim yellow "\u26a0 " (card/suit-symbols immune-suit) " "
                                   (suit-power-labels immune-suit)
                                   " — blocked by enemy immunity" ansi-reset))))
         selection-preview (when (and selector-state
                                      (seq (:selected selector-state)))
                             (render-selection-preview
                               hand (:selected selector-state)
                               current-enemy phase))]
     (str/join "\n"
       (remove nil?
         [""
          "=== REGICIDE ==="
          ""
          "Current Enemy:"
          (render-enemy current-enemy)
          ""
          (str "  Tavern: " (count tavern-deck) " cards"
               "  |  Discard: " (count discard-pile) " cards"
               "  |  Enemies remaining: " enemies-remaining
               "  |  Jesters: " (or (:jesters state) 0))
          ""
          (render-phase-banner phase current-enemy)
          ""
          (when (> (:num-players state) 1)
            (render-other-players state))
          (str (if (> (:num-players state) 1)
                 (str "Player " (inc (:current-player state)) "'s Hand")
                 "Your Hand")
               " (" (sort-order-labels sort-order) "):")
          (str "  " hand-display)
          cursor-warning
          selection-preview
          ""])))))

(defn render-selector-prompt
  ([phase multiplayer?] (render-selector-prompt phase multiplayer? true))
  ([phase multiplayer? show-jester?]
   (let [base (str "\u2190\u2192 move  \u2191 select  Enter "
                   (case phase :play-cards "play" :suffer-damage "discard" "?"))
         yield-part (when (and multiplayer? (= phase :play-cards)) "  y yield")
         jester-part (when show-jester? "  j jester")
         rest-part "  p sort  h help  q quit"]
     (str base (or yield-part "") (or jester-part "") rest-part))))

(defn render-help []
  (str/join "\n"
    [""
     "=== HELP ==="
     ""
     "  Controls:"
     "    \u2190 \u2192     - Move cursor left/right"
     "    \u2191        - Toggle card selection"
     "    Enter    - Play/discard selected cards"
     "    y        - Yield turn (multiplayer only, pass to next player)"
     "    j        - Use a jester (solo: refresh hand; multiplayer: cancel immunity)"
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
     "  Exact kill: If damage equals remaining health exactly, enemy goes on top of draw pile"
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


(defn- render-cancelled-powers
  "Render lines for suit powers that were cancelled by enemy immunity."
  [immune-suit played-suits]
  (when (and immune-suit (contains? played-suits immune-suit))
    [(str "  " dim strikethrough (card/suit-symbols immune-suit) " "
          (suit-power-labels immune-suit)
          ansi-reset dim " (immune)" ansi-reset)]))

(defn render-action-result [action-info]
  (when action-info
    (if (:jester-used action-info)
      (str "  " yellow "\ud83c\udccf Jester used! Hand refreshed." ansi-reset)
      (let [immune (:immune-suit action-info)
            played-suits (:played-suits action-info)
            lines (concat
                    [(when-let [cards (:played action-info)]
                     (str "  Played: " (str/join " " (map card/card-label cards))))
                   (when-let [dmg (:damage action-info)]
                     (str "  Damage dealt: " dmg))
                   (when (pos? (or (:attack-reduce action-info) 0))
                     (str "  " (card/suit-symbols :spades) " Reduced enemy attack by " (:attack-reduce action-info)))
                   (when (pos? (or (:hearts-heal action-info) 0))
                     (str "  " (card/suit-symbols :hearts) " Recycled " (:hearts-heal action-info) " cards from discard"))
                   (when (pos? (or (:diamonds-draw action-info) 0))
                     (str "  " (card/suit-symbols :diamonds) " Drew " (:diamonds-draw action-info) " cards"))]
                  (render-cancelled-powers immune played-suits)
                  [(when (:enemy-defeated action-info)
                     (if (:exact-kill action-info)
                       "  >> Enemy defeated! (Exact kill - enemy placed on top of draw pile!)"
                       "  >> Enemy defeated!"))
                   (when-let [discarded (:discarded action-info)]
                     (str "  Discarded to absorb damage: " (str/join " " (map card/card-label discarded))))])]
        (str/join "\n" (remove nil? lines))))))

(defn render-enemy-defeated
  "Render an interstitial screen when an enemy is defeated."
  [action-info defeated-card next-enemy enemies-left]
  (let [green "\u001b[32m"]
    (str/join "\n"
      (remove nil?
        [""
         (str green term/bold "=== ENEMY DEFEATED ===" ansi-reset)
         ""
         (render-action-result action-info)
         ""
         (str "  Enemies remaining: " enemies-left)
         (when next-enemy
           (str "\n  Next enemy:\n" (render-enemy next-enemy)))
         ""
         "  Press any key to continue..."]))))
