(ns regicide.enemy)

(def enemy-stats
  {11 {:health 20 :attack 10}
   12 {:health 30 :attack 15}
   13 {:health 40 :attack 20}})

(defn make-enemy [card]
  (let [{:keys [health attack]} (enemy-stats (:rank card))]
    {:card       card
     :health     health
     :attack     attack
     :max-health health
     :max-attack attack}))

(defn apply-damage [enemy amount]
  (update enemy :health - amount))

(defn reduce-attack [enemy amount]
  (update enemy :attack #(max 0 (- % amount))))

(defn defeated? [enemy]
  (<= (:health enemy) 0))

(defn exact-kill? [enemy damage]
  (= damage (:health enemy)))

(defn immune-to-suit? [enemy suit]
  (= (get-in enemy [:card :suit]) suit))
