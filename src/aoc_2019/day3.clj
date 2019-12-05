(ns aoc-2019.day3
  (:require [clojure.string :as str]))

(defn- getNumFromDirection
  [direction]
  (Integer. (subs direction 1)))

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
    (str/starts-with? direction "R") (assoc location :x (inc (:x location)))
    (str/starts-with? direction "U") (assoc location :y (inc (:y location)))
    (str/starts-with? direction "L") (assoc location :x (dec (:x location)))
    (str/starts-with? direction "D") (assoc location :y (dec (:y location)))))

(defn- addPoint
  [locations point step]
  (let [x (:x point)
        y (:y point)
        pointsForX (get locations x)]
    (cond (not pointsForX) (assoc locations x {y step})
          (not (contains? pointsForX y)) (assoc locations x (assoc pointsForX y step))
          :else locations)))

(defn- stepThroughPointsRecur
  [directions current step locations]
  (cond
    (empty? directions) locations
    :else (recur (decrementDirections directions)
                 (getNextStep current (first directions))
                 (inc step)
                 (addPoint locations current step))))

(defn- stepThroughPoints
  [directions]
  (stepThroughPointsRecur directions {:x 0 :y 0} 0 {}))

(defn- getCommonYs
  [ys other]
  (filter #(contains? other %) ys))

(defn- getIntersectingPoints
  [x first second]
  (let [ys (map #(get % 0) (seq first))
        commonYs (getCommonYs ys second)]
    (if (empty? commonYs)
      []
      (map (fn [y] [x y (+ (get first y) (get second y))]) commonYs))))

(defn- findPointIntersectionRecur
  [firstPoints secondPoints xsToCheck common]
  (cond
    (empty? xsToCheck) common
    (contains? secondPoints (first xsToCheck)) (recur firstPoints
                                                      secondPoints
                                                      (rest xsToCheck)
                                                      (apply conj common
                                                            (getIntersectingPoints (first xsToCheck)
                                                                                   (get firstPoints (first xsToCheck))
                                                                                   (get secondPoints (first xsToCheck)))))
    :else (recur firstPoints secondPoints (rest xsToCheck) common)))

(defn- filterOutOrigin
  [points]
  (filter #(and (not= (get % 0) 0) (not= (get % 1) 0)) points))

(defn- findPointIntersection
  [firstPoints secondPoints]
  (filterOutOrigin (findPointIntersectionRecur firstPoints secondPoints (map #(get % 0) (seq firstPoints)) [])))

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
        firstPoints (stepThroughPoints firstDirections)
        secondPoints (stepThroughPoints secondDirections)]
    (apply min (map #(manhattanDistance %) (findPointIntersection firstPoints secondPoints)))))

(defn puzzle2
  [inputs]
  (let [bothDirectionStrings (str/split inputs #"\n")
        firstDirections (str/split (get bothDirectionStrings 0) #",")
        secondDirections (str/split (get bothDirectionStrings 1) #",")
        firstPoints (stepThroughPoints firstDirections)
        secondPoints (stepThroughPoints secondDirections)]
    (apply min (map #(get % 2) (findPointIntersection firstPoints secondPoints)))))
