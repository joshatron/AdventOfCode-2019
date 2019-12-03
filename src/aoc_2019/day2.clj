(ns aoc-2019.day2
  (:require [clojure.string :as str]))

(defn- getAddressValue
  "Gets value at memory address"
  [program address]
  (get program (get program address)))

(defn- processTwoOperatorsToResult
  "Process operation of third = first op second"
  [program op startAddress]
  (let [firstAddress (+ startAddress 1)
        secondAddress (+ startAddress 2)
        resultAddress (+ startAddress 3)]
    (assoc program
      (get program resultAddress)
      (op (getAddressValue program firstAddress) (getAddressValue program secondAddress)))))

(defn- processAdd
  "Process add operation"
  [program startAddress]
  (processTwoOperatorsToResult program + startAddress))

(defn- processMultiply
  "Process multiply operation"
  [program startAddress]
  (processTwoOperatorsToResult program * startAddress))

(defn- processProgram
  "Go through sequence till program halt"
  [program address]
  (let [op (get program address)]
    (cond
      (= op 99) program
      (= op 1) (processProgram (processAdd program address) (+ address 4))
      (= op 2) (processProgram (processMultiply program address) (+ address 4)))))

(defn- processInputs
  "Try specific pairing of inputs"
  [program input1 input2]
  (first (processProgram (assoc program 1 input1 2 input2) 0)))

(defn puzzle1
  "Main function to get output from inputs for day 2 puzzle 1"
  [input]
  (processInputs (mapv #(Integer. %) (str/split input #",")) 12 2))

(defn- recurseInputs
  "Recursive call to get correct inputs"
  [program input1 input2]
  (let [result (processInputs program input1 input2)]
    ;(println input1 input2)
    (cond
      (= result 19690720) (+ (* 100 input1) input2)
      (= input1 99) (recur program 0 (inc input2))
      :else (recur program (inc input1) input2))))

(defn puzzle2
  "Main function to get output from inputs for day 2 puzzle 2"
  [input]
  (recurseInputs (mapv #(Integer. %) (str/split input #",")) 0 0))
