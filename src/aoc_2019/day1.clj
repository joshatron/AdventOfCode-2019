(ns aoc-2019.day1
  (:require [clojure.string :as str]))

(defn- calculateFuelForMass
  "Calculate answer for one input"
  [input]
  (max 0 (- (quot input 3) 2)))

(defn- calculateFuelForMassRecursive
  "Recursively calculates for fuel mass"
  [input]
  (let [ masses (iterate calculateFuelForMass input)]
    (reduce +
            (next (take-while #(not= 0 %) masses)))))


(defn sumInputs
  "Calulates output for each input and sums the result"
  [inputs algorithm]
  (reduce +
          (map #(algorithm (Integer. %))
               (str/split inputs #"\n"))))

(defn puzzle1
  "Main function to get output from inputs for day 1 puzzle 1"
  [inputs]
  (sumInputs inputs calculateFuelForMass))

(defn puzzle2
  "Main function to get output from inputs for day 1 puzzle 2"
  [inputs]
  (sumInputs inputs calculateFuelForMassRecursive))
