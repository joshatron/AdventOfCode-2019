(ns aoc-2019.day2
  (:require [aoc-2019.intcomp :as ic]))

(defn- processInputs
  [program input1 input2]
  (first (:program (ic/process-program-till-halt-or-input (assoc program 1 input1 2 input2) [] [] 0 0))))

(defn puzzle1
  [input]
  (processInputs (ic/string-to-program input) 12 2))

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
  (recurseInputs (ic/string-to-program input) 0 0))
