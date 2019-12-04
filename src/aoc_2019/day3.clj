(ns aoc-2019.day3
  (:require [clojure.string :as str]))

(defn- getNumFromDirection
  "Extracts distance number from direction"
  [direction]
  (Integer. (subs direction 1)))

(defn- addPoint
  "Add point to points map"
  [points x y]
  (if (contains? points x)
    (assoc points x (conj (get points x) y))
    (assoc points x [y])))

(defn- getPointsForDirection
  "Adds points to map for direction"
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
  "Find the next start location"
  [start direction]
  (cond
    (str/starts-with? direction "R") [(+ (get start 0) (getNumFromDirection direction)) (get start 1)]
    (str/starts-with? direction "U") [(get start 0) (+ (get start 1) (getNumFromDirection direction))]
    (str/starts-with? direction "L") [(- (get start 0) (getNumFromDirection direction)) (get start 1)]
    (str/starts-with? direction "D") [(get start 0) (- (get start 1) (getNumFromDirection direction))]))

(defn- getPointsMap
  "Creates map with x as key and list of y as value"
  [directions points start]
  (cond
    (empty? directions) points
    :else (recur (rest directions) (getPointsForDirection (first directions) points start) (getNextLocation start (first directions)))))

(defn- intersect
  "Intersect two lists"
  [l1 l2]
  (filter #(some #{%} l2) l1))

(defn- findCommonForX
  "Gets common points for x and ys"
  [x y1 y2]
  (map (fn [y] [x y]) (intersect y1 y2)))

(defn- findCommonRecur
  "Finds the common points between two point maps"
  [f s xs common]
  (cond
    (empty? xs) common
    :else (recur f
                 s
                 (rest xs)
                 (apply conj common (findCommonForX (first xs)
                                                    (get f (first xs))
                                                    (get s (first xs)))))))

(defn- findCommon
  "Finds the common points between two point maps"
  [first second]
  (filter #(and (not= (get % 0) 0) (not= (get % 1) 0)) (findCommonRecur first second (map #(get % 0) (seq first)) [])))

(defn abs [n] (max n (- n)))

(defn puzzle1
  "Main function to get output from inputs for day 3 puzzle 1"
  [inputs]
  (apply min (map #(+ (abs (get % 0)) (abs (get % 1)))
                  (apply findCommon (map #(getPointsMap (str/split % #",") {} [0 0])
                                         (str/split inputs #"\n"))))))
