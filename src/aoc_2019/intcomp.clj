(ns aoc-2019.intcomp
  (:require [clojure.string :as str]))

(defn- getAddressValue
  [program address]
  (get program (get program address)))

(defn- getAddressOrValue
  [program address type]
  (if type
    (get program address)
    (getAddressValue program address)))

(defn- processTwoOperatorsToResult
  [program op startAddress firstType secondType]
  (let [firstAddress (+ startAddress 1)
        secondAddress (+ startAddress 2)
        resultAddress (+ startAddress 3)]
    (assoc program
      (get program resultAddress)
      (op (getAddressOrValue program firstAddress firstType) (getAddressOrValue program secondAddress secondType)))))

(defn- processAdd
  [program startAddress firstType secondType]
  (processTwoOperatorsToResult program + startAddress firstType secondType))

(defn- processMultiply
  [program startAddress firstType secondType]
  (processTwoOperatorsToResult program * startAddress firstType secondType))

(defn- processGetInput
  [program startAddress input]
  (assoc program
    (get program (+ startAddress 1))
    input))

(defn- processLessThan
  [program startAddress firstType secondType]
    (assoc program
      (get program (+ startAddress 3))
      (if (< (getAddressOrValue program (+ startAddress 1) firstType) (getAddressOrValue program (+ startAddress 2) secondType))
        1
        0)))

(defn- processEqual
  [program startAddress firstType secondType]
  (assoc program
    (get program (+ startAddress 3))
    (if (= (getAddressOrValue program (+ startAddress 1) firstType) (getAddressOrValue program (+ startAddress 2) secondType))
      1
      0)))

(defn getFinalProgramState
  [program address]
  (let [op (get program address)]
    (cond
      (= op 99) program
      (= op 1) (recur (processAdd program address false false) (+ address 4))
      (= op 2) (recur (processMultiply program address false false) (+ address 4)))))

(defn- parseOp
  [op]
  (let [opStr (str "0000" op)
        opCode (subs opStr (- (count opStr) 2))
        parameterModes (subs opStr 0 (- (count opStr) 2))]
    {:op (Integer. opCode)
     :first (and (> (count parameterModes) 0) (= (subs parameterModes (- (count parameterModes) 1)) "1"))
     :second (and (> (count parameterModes) 1) (= (subs parameterModes (- (count parameterModes) 2) (- (count parameterModes) 1)) "1"))}))

(defn getProgramOutput
  [program inputs outputs address]
  (let [op (parseOp (get program address))]
    (cond
      (= (:op op) 99) outputs
      (= (:op op) 1) (recur (processAdd program address (:first op) (:second op)) inputs outputs (+ address 4))
      (= (:op op) 2) (recur (processMultiply program address (:first op) (:second op)) inputs outputs (+ address 4))
      (= (:op op) 3) (recur (processGetInput program address (first inputs)) (rest inputs) outputs (+ address 2))
      (= (:op op) 4) (recur program inputs (conj outputs (getAddressOrValue program (+ address 1) (:first op))) (+ address 2))
      (= (:op op) 5) (recur program inputs outputs (if (not= (getAddressOrValue program (+ address 1) (:first op)) 0)
                                                     (getAddressOrValue program (+ address 2) (:second op))
                                                     (+ address 3)))
      (= (:op op) 6) (recur program inputs outputs (if (= (getAddressOrValue program (+ address 1) (:first op)) 0)
                                                     (getAddressOrValue program (+ address 2) (:second op))
                                                     (+ address 3)))
      (= (:op op) 7) (recur (processLessThan program address (:first op) (:second op)) inputs outputs (+ address 4))
      (= (:op op) 8) (recur (processEqual program address (:first op) (:second op)) inputs outputs (+ address 4)))))

(defn processProgramTillHaltOrInput
  [program inputs outputs address]
  (let [op (parseOp (get program address))]
    (cond
      (= (:op op) 99) {:program program :output outputs :done true :address address}
      (= (:op op) 1) (recur (processAdd program address (:first op) (:second op)) inputs outputs (+ address 4))
      (= (:op op) 2) (recur (processMultiply program address (:first op) (:second op)) inputs outputs (+ address 4))
      (= (:op op) 3) (if (empty? inputs)
                       {:program program :output outputs :done false :address address}
                       (recur (processGetInput program address (first inputs)) (rest inputs) outputs (+ address 2)))
      (= (:op op) 4) (recur program inputs (conj outputs (getAddressOrValue program (+ address 1) (:first op))) (+ address 2))
      (= (:op op) 5) (recur program inputs outputs (if (not= (getAddressOrValue program (+ address 1) (:first op)) 0)
                                                     (getAddressOrValue program (+ address 2) (:second op))
                                                     (+ address 3)))
      (= (:op op) 6) (recur program inputs outputs (if (= (getAddressOrValue program (+ address 1) (:first op)) 0)
                                                     (getAddressOrValue program (+ address 2) (:second op))
                                                     (+ address 3)))
      (= (:op op) 7) (recur (processLessThan program address (:first op) (:second op)) inputs outputs (+ address 4))
      (= (:op op) 8) (recur (processEqual program address (:first op) (:second op)) inputs outputs (+ address 4)))))

(defn stringToProgram
  [str]
  (mapv #(Integer. %) (str/split str #",")))