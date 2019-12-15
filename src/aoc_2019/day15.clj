(ns aoc-2019.day15
  (:require [aoc-2019.intcomp :as ic]))

(defn- move-loc [loc dir]
  (case dir
    1 {:x (:x loc) :y (inc (:y loc))}
    2 {:x (:x loc) :y (dec (:y loc))}
    3 {:x (dec (:x loc)) :y (:y loc)}
    4 {:x (inc (:x loc)) :y (:y loc)}))

(defn- try-move [state dir visited]
  (let [moved-program (ic/process-program-till-halt-or-input (:program state) [dir])]
    (cond
      (= (first (:output moved-program)) 0) nil
      (some #{(move-loc (:loc state) dir)} visited) nil
      :else {:program moved-program :loc (move-loc (:loc state) dir)})))

(defn- get-possible [state visited] (remove nil? (map #(try-move state % visited) [1 2 3 4])))

(defn- final-spot [state] (= (first (:output (:program state))) 2))

(defn- depth-first-find-location
  ([input] (depth-first-find-location {:program (ic/string-to-program input) :loc {:x 0 :y 0}} []))
  ([state visited]
   (let [next (get-possible state visited)]
    (cond
      (empty? next) nil
      (some final-spot next) (first (filter final-spot next))
      :else (some #(depth-first-find-location % (conj visited (:loc state))) next)
      ))))

(defn puzzle1 [input]
  (depth-first-find-location input))

(defn puzzle2 [input])
