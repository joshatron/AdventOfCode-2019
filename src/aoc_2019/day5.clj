(ns aoc-2019.day5
  (:require [clojure.string :as str]
            [aoc-2019.intcomp :as ic]))

(defn puzzle1
  [input]
  (last (ic/getProgramOutput (mapv #(Integer. %) (str/split input #",")) [1] [] 0)))

(defn puzzle2
  [input]
  (ic/getProgramOutput (mapv #(Integer. %) (str/split input #",")) [1] [] 0))
