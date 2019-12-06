(ns aoc-2019.day2
  (:require [clojure.string :as str]
            [aoc-2019.intcomp :as ic]))

(defn- processInputs
  [program input1 input2]
  (first (ic/getFinalProgramState (assoc program 1 input1 2 input2) 0)))

(defn puzzle1
  [input]
  (processInputs (mapv #(Integer. %) (str/split input #",")) 12 2))

(defn- recurseInputs
  [program input1 input2]
  (let [result (processInputs program input1 input2)]
    ;(println input1 input2)
    (cond
      (= result 19690720) (+ (* 100 input1) input2)
      (= input1 99) (recur program 0 (inc input2))
      :else (recur program (inc input1) input2))))

(defn puzzle2
  [input]
  (recurseInputs (mapv #(Integer. %) (str/split input #",")) 0 0))
