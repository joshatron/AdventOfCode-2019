(ns aoc-2019.day3
  (:require [clojure.string :as str]))

(defn- getNumFromDirection
  [direction]
  (Integer. (subs direction 1)))

(defn- addPoint
  [points x y]
  (if (contains? points x)
    (assoc points x (conj (get points x) y))
    (assoc points x [y])))

(defn- getPointsForDirection
  [direction points start]
  (let [dir (subs direction 0 1)
        dist (getNumFromDirection direction)]
    (cond
      (= dist 0) (addPoint points (get start 0) (get start 1))
      (= dir "R") (recur (str dir (dec dist)) (addPoint points (get start 0) (get start 1)) [(inc (get start 0)) (get start 1)])
      (= dir "U") (recur (str dir (dec dist)) (addPoint points (get start 0) (get start 1)) [(get start 0) (inc (get start 1))])
      (= dir "L") (recur (str dir (dec dist)) (addPoint points (get start 0) (get start 1)) [(dec (get start 0)) (get start 1)])
      (= dir "D") (recur (str dir (dec dist)) (addPoint points (get start 0) (get start 1)) [(get start 0) (dec (get start 1))]))))

(defn- getNextLocation
  [start direction]
  (cond
    (str/starts-with? direction "R") [(+ (get start 0) (getNumFromDirection direction)) (get start 1)]
    (str/starts-with? direction "U") [(get start 0) (+ (get start 1) (getNumFromDirection direction))]
    (str/starts-with? direction "L") [(- (get start 0) (getNumFromDirection direction)) (get start 1)]
    (str/starts-with? direction "D") [(get start 0) (- (get start 1) (getNumFromDirection direction))]))

(defn- getPointsMap
  [directions points start]
  (cond
    (empty? directions) points
    :else (recur (rest directions)
                 (getPointsForDirection (first directions) points start)
                 (getNextLocation start (first directions)))))

(defn- intersect
  [l1 l2]
  (filter #(some #{%} l2) l1))

(defn- findCommonForX
  [x y1 y2]
  (map (fn [y] [x y]) (intersect y1 y2)))

(defn- findCommonRecur
  [firstPoints secondPoints xs common]
  (cond
    (empty? xs) common
    :else (recur firstPoints
                 secondPoints
                 (rest xs)
                 (apply conj common (findCommonForX (first xs)
                                                    (get firstPoints (first xs))
                                                    (get secondPoints (first xs)))))))

(defn- filterOutOrigin
  [points]
  (filter #(and (not= (get % 0) 0) (not= (get % 1) 0)) points))

(defn- findCommon
  [first second]
  (let [firstPoints (getPointsMap first {} [0 0])
        secondPoints (getPointsMap second {} [0 0])]
    (filterOutOrigin (findCommonRecur firstPoints
                                      secondPoints
                                      (map #(get % 0) (seq firstPoints))
                                      []))))

(defn- abs
  [n]
  (max n (- n)))

(defn- manhattanDistance
  [point]
  (+ (abs (get point 0)) (abs (get point 1))))

(defn puzzle1
  [inputs]
  (let [bothDirectionStrings (str/split inputs #"\n")
        firstDirections (str/split (get bothDirectionStrings 0) #",")
        secondDirections (str/split (get bothDirectionStrings 1) #",")
        commonPoints (findCommon firstDirections secondDirections)]
(apply min (map #(manhattanDistance %)
                    commonPoints))))

(defn- decrementDirections
  [directions]
  (let [dir (subs (first directions) 0 1)
        dist (getNumFromDirection (first directions))]
    (if (= dist 1)
      (subvec directions 1)
      (assoc directions 0 (str dir (dec dist))))))

(defn- getNextStep
  [location direction]
  (cond
    (str/starts-with? direction "R") [(inc (get location 0)) (get location 1)]
    (str/starts-with? direction "U") [(get location 0) (inc (get location 1))]
    (str/starts-with? direction "L") [(dec (get location 0)) (get location 1)]
    (str/starts-with? direction "D") [(get location 0) (dec (get location 1))]))

(defn- getStepsToPoint
  [point current directions steps]
  (if (= point current)
    steps
    (recur point
           (getNextStep current (first directions))
           (decrementDirections directions)
           (inc steps))))

(defn- getTotalStepsToPoint
  [point firstDirections secondDirections]
  (+ (getStepsToPoint point [0 0] firstDirections 0)
     (getStepsToPoint point [0 0] secondDirections 0)))

(defn puzzle2
  [inputs]
  (let [bothDirectionStrings (str/split inputs #"\n")
        firstDirections (str/split (get bothDirectionStrings 0) #",")
        secondDirections (str/split (get bothDirectionStrings 1) #",")
        commonPoints (findCommon firstDirections secondDirections)]
    (apply min (map #(getTotalStepsToPoint % firstDirections secondDirections)
                    commonPoints))))
