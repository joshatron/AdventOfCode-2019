(ns aoc-2019.day11
  (:require [aoc-2019.intcomp :as ic]))

(defn- get-new-dir [original rotate-dir]
  (let [new-dir (if (= rotate-dir 0) (dec original) (inc original))]
    (cond
      (< new-dir 0) (+ new-dir 4)
      (> new-dir 3) (- new-dir 4)
      :else new-dir)))

(defn- move-in-dir [robot-state]
  (case (:dir robot-state)
    0 {:x (:x robot-state) :y (inc (:y robot-state)) :dir (:dir robot-state)}
    1 {:x (inc (:x robot-state)) :y (:y robot-state) :dir (:dir robot-state)}
    2 {:x (:x robot-state) :y (dec (:y robot-state)) :dir (:dir robot-state)}
    3 {:x (dec (:x robot-state)) :y (:y robot-state) :dir (:dir robot-state)}))

(defn- move-robot [start-loc rotate-dir]
  (move-in-dir {:x (:x start-loc) :y (:y start-loc) :dir (get-new-dir (:dir start-loc) rotate-dir)}))

(defn- find-location [locations location]
  (first (filter #(and (= (:x location) (:x %)) (= (:y location) (:y %))) locations)))

(defn- get-color-of-loc [locations location]
  (let [matching (find-location locations location)]
    (if (nil? matching)
      0
      (:color matching))))

(defn- add-location [locations location paint-color]
  (conj (filter #(or (not= (:x location) (:x %)) (not= (:y location) (:y %))) locations)
        {:x (:x location) :y (:y location) :color paint-color}))

(defn- update-painted [locations location paint-color newly-painted]
  (if (and (not=
             (get-color-of-loc locations location)
             paint-color) (empty? (filter #(and (= (:x location) (:x %)) (= (:y location) (:y %))) locations)))
    (inc newly-painted)
    newly-painted))

;Directions
;  0
;3   1
;  2
(defn- get-locations-painted
  ([program initial]
   (get-locations-painted (ic/process-program-till-halt-or-input program initial) {:x 0 :y 0 :dir 0} [] 0))
  ([program robot-state locations newly-painted]
   (if (:done program)
     locations
     (recur (ic/process-program-till-halt-or-input program [(get-color-of-loc locations (move-robot robot-state (last (:output program))))])
            (move-robot robot-state (last (:output program)))
            (add-location locations robot-state (first (:output program)))
            (update-painted locations robot-state (first (:output program)) newly-painted)))))

(defn puzzle1 [input]
  (count (get-locations-painted (ic/string-to-program input) [0])))

(defn- draw-locs [locations min-x min-y max-x max-y x y string]
  (cond
    (< y min-y) string
    (> x max-x) (do
                  (recur locations min-x min-y max-x max-y min-x (dec y) (str string "\n")))
    :else (do
            (recur locations min-x min-y max-x max-y (inc x) y
                   (str string (if (= (get-color-of-loc locations {:x x :y y}) 1) "#" " "))))))

(defn puzzle2 [input]
  (let [locations (get-locations-painted (ic/string-to-program input) [1])
        min-x (apply min (map #(:x %) locations))
        max-x (apply max (map #(:x %) locations))
        min-y (apply min (map #(:y %) locations))
        max-y (apply max (map #(:y %) locations))]
    (str "\n" (draw-locs locations min-x min-y max-x max-y min-x max-y ""))))
