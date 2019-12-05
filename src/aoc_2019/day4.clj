(ns aoc-2019.day4
  (:require [clojure.string :as str]))

(defn- createRange
  [start end]
  (take (- end start) (iterate inc start)))

(defn- getIntFromChar
  [numberString index]
  (Integer. (subs numberString index (inc index))))

(defn- identicalAdjacent
  [numberString]
  (cond
    (= (count numberString) 1) false
    (= (getIntFromChar numberString 0) (getIntFromChar numberString 1)) true
    :else (recur (subs numberString 1))))

(defn- increasing
  [numberString]
  (cond
    (= (count numberString) 1) true
    (> (getIntFromChar numberString 0) (getIntFromChar numberString 1)) false
    :else (recur (subs numberString 1))))

(defn- legalNumberPuzzle1
  [number]
  (let [numberStr (str number)]
    (and
      (identicalAdjacent numberStr)
      (increasing numberStr))))

(defn puzzle1
  [input]
  (let [numbers (str/split input #"-")
        start (Integer. (get numbers 0))
        end (Integer. (get numbers 1))]
    (count (filter legalNumberPuzzle1 (createRange start end)))))

(defn- getMatchingFromBeginning
  [numberString total]
  (cond
    (= (count numberString) 1) total
    (not= (getIntFromChar numberString 0) (getIntFromChar numberString 1)) total
    :else (recur (subs numberString 1) (inc total))))

(defn- identicalAdjacentOnlyTwo
  [numberString]
  (let [matching (getMatchingFromBeginning numberString 1)]
    (cond
      (= matching 2) true
      (= (count numberString) matching) false
      :else (recur (subs numberString matching)))))

(defn- legalNumberPuzzle2
  [number]
  (let [numberStr (str number)]
    (and
      (identicalAdjacentOnlyTwo numberStr)
      (increasing numberStr))))

(defn puzzle2
  [input]
  (let [numbers (str/split input #"-")
        start (Integer. (get numbers 0))
        end (Integer. (get numbers 1))]
    (count (filter legalNumberPuzzle2 (createRange start end)))))
