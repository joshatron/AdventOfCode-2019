(ns aoc-2019.day12
  (:require [clojure.string :as str]))

(defn- to-position-map [components]
  {:x (first components) :y (first (rest components)) :z (first (rest (rest components)))})

(defn- get-initial-moon-position [line]
  (let [minus-tags (subs line 1 (dec (count line)))
        components (map #(Integer. (get (str/split (str/trim %) #"=") 1)) (str/split minus-tags #","))]
    (to-position-map components)))

(defn- get-initial-moon-state [line]
  {:vel {:x 0 :y 0 :z 0}
   :pos (get-initial-moon-position line)})

(defn- get-initial-states [input]
  (map get-initial-moon-state (str/split input #"\n")))

(defn- update-component [original main other]
  (cond
    (< main other) (inc original)
    (> main other) (dec original)
    :else original))

(defn- update-to-moon [main other]
  {:x (update-component (:x (:vel main)) (:x (:pos main)) (:x (:pos other)))
   :y (update-component (:y (:vel main)) (:y (:pos main)) (:y (:pos other)))
   :z (update-component (:z (:vel main)) (:z (:pos main)) (:z (:pos other)))})

(defn- update-velocity [state moon] (reduce #(assoc %1 :vel (update-to-moon %1 %2)) moon state))

(defn- update-velocities [state] (map #(update-velocity state %) state))

(defn- update-to-velocity [pos vel]
  {:x (+ (:x pos) (:x vel))
   :y (+ (:y pos) (:y vel))
   :z (+ (:z pos) (:z vel))})

(defn- update-position [moon]
  (let [vel (:vel moon)
        pos (:pos moon)]
    {:vel vel
     :pos (update-to-velocity pos vel)}))

(defn- update-positions [state] (map update-position state))

(defn- walk-step [state] (update-positions (update-velocities state)))

(defn- walk-steps [state steps]
  (if (= steps 0)
    state
    (recur (walk-step state) (dec steps))))

(defn- get-energy [xyz] (+ (Math/abs (:x xyz)) (Math/abs (:y xyz)) (Math/abs (:z xyz))))

(defn- get-energy-of-moon [moon] (* (get-energy (:vel moon)) (get-energy (:pos moon))))

(defn- get-total-energy [state] (reduce + (map get-energy-of-moon state)))

(defn puzzle1 [input]
  (get-total-energy (walk-steps (get-initial-states input) 1000)))
