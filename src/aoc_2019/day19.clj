(ns aoc-2019.day19
  (:require [aoc-2019.intcomp :as ic]))

(defn- create-points [width height]
  (map (fn [n] {:x (mod n width) :y (quot n height)}) (take (* width height) (iterate inc 0))))

(defn puzzle1 [input]
  (let [program (ic/string-to-program input)]
    (->> (create-points 50 50)
         (map #(ic/process-program-till-halt-or-input program [(:x %) (:y %)]))
         (map :output)
         (map first)
         (filter #(= % 1))
         (count))))

(defn puzzle2 [input])
