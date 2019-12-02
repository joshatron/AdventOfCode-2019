(ns aoc-2019.day1
  (:require [aoc-2019.utils :as utils]))

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

(defn puzzle1
  "Main function to get output from inputs for day 1 puzzle 1"
  [inputs]
  (utils/sumInputs inputs calculateFuelForMass))

(defn puzzle2
  "Main function to get output from inputs for day 1 puzzle 2"
  [inputs]
  (utils/sumInputs inputs calculateFuelForMassRecursive))
