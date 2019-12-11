(ns aoc-2019.day10
  (:require [clojure.string :as str])
  (:import (clojure.lang Numbers)))

(defn- asteroid-char-to-number [asteroid-char]
  (if (= asteroid-char \#) 1 0))

(defn- convert-line [line]
  (mapv asteroid-char-to-number (vec line)))

(defn- to-chart [input]
  (mapv convert-line (str/split input #"\n")))

(defn- asteroid-at-location? [chart location]
  (= 1 (get (get chart (:y location)) (:x location))))

(defn- chart-width [chart] (count (first chart)))
(defn- chart-height [chart] (count chart))
(defn- num-locations [chart] (* (chart-width chart) (chart-height chart)))
(defn- num-to-location [chart num]
  {:x (mod num (chart-width chart))
   :y (quot num (chart-width chart))})

(defn- get-locations [chart]
  (filter #(asteroid-at-location? chart %) (take (num-locations chart) (map #(num-to-location chart %) (iterate inc 0)))))

(defn abs [n] (max n (- n)))

(defn- get-point-ratio [loc1 loc2] (abs (/ (- (:x loc1) (:x loc2)) (- (:y loc1) (:y loc2)))))

(defn- get-plus-minus [p1 p2] (if (< p1 p2) 1 -1))

(defn- get-delta-between-points [loc1 loc2]
  (cond
    (= loc1 loc2) {:x 0 :y 0}
    (= (- (:x loc1) (:x loc2)) 0) {:x 0 :y (* 1 (get-plus-minus (:y loc1) (:y loc2)))}
    (= (- (:y loc1) (:y loc2)) 0) {:x (* 1 (get-plus-minus (:x loc1) (:x loc2))) :y 0}
    :else {:x (* (numerator (Numbers/toRatio (rationalize (get-point-ratio loc1 loc2)))) (get-plus-minus (:x loc1) (:x loc2)))
           :y (* (denominator (Numbers/toRatio (rationalize (get-point-ratio loc1 loc2)))) (get-plus-minus (:y loc1) (:y loc2)))}))

(defn- take-step [loc delta]
  {:x (+ (:x loc) (:x delta))
   :y (+ (:y loc) (:y delta))})

(defn- any-step-has-asteroid? [chart loc1 loc2 delta]
  (let [next-loc (take-step loc1 delta)]
    (cond
      (= next-loc loc2) true
      (asteroid-at-location? chart next-loc) false
      :else (recur chart next-loc loc2 delta))))

(defn- visible? [chart loc1 loc2]
  (any-step-has-asteroid? chart loc1 loc2 (get-delta-between-points loc1 loc2)))

(defn- get-visible-for-location [chart locations location]
  {:loc location
   :visible (dec (count (filter true? (map #(visible? chart location %) locations))))})

(defn- get-visible-for-all [chart]
  (let [locations (get-locations chart)]
    (map #(get-visible-for-location chart locations %) locations)))

(defn- get-most-visible [chart]
  (reduce #(if (> (:visible %1) (:visible %2)) %1 %2) (get-visible-for-all chart)))

(defn puzzle1 [input]
  (:visible (get-most-visible (to-chart input))))

(defn- remove-location [locations location] (remove #(= location %) locations))

(defn- get-atan [x y] (Math/atan (double (/ y x))))

(defn- get-angle [location]
  (let [x (:x location)
        y (* (:y location) -1)]
    (cond
      (and (= x 0) (> y 0)) 0
      (and (= x 0) (< y 0)) Math/PI
      (and (= y 0) (< x 0)) (* 1.5 Math/PI)
      (and (= y 0) (> x 0)) (* 0.5 Math/PI)
      (> x 0) (- (/ Math/PI 2) (get-atan x y))
      (< x 0) (+ Math/PI (- (/ Math/PI 2) (get-atan x y))))))

(defn- get-angle-between [reference other]
  (let [reference-angle (get-angle reference)
        other-angle (get-angle other)]
    (if (< reference-angle other-angle)
      (- other-angle reference-angle)
      (- (+ (* Math/PI 2) other-angle) reference-angle))))

(defn- get-rotation-for-location [center other current-dir]
  (let [delta (get-delta-between-points center other)]
    (if (= delta current-dir)
      (* 2 Math/PI)
      (get-angle-between current-dir delta))))

(defn- get-next-destroyed [others center current-dir]
  (:loc (reduce #(if (< (:rotation %1) (:rotation %2)) %1 %2)
                (map (fn [l] {:loc l :rotation (get-rotation-for-location center l current-dir)}) others))))

(defn- get-x-destroyed [others center current-dir num]
  (let [next-location (get-next-destroyed others center current-dir)]
  (if (= num 0)
    next-location
    (recur (remove-location others next-location)
           center
           (get-delta-between-points center next-location)
           (dec num)))))

(defn puzzle2 [input]
  (let [chart (to-chart input)
        station (:loc (get-most-visible chart))
        locations (remove-location (get-locations chart) station)
        last-destroyed(get-x-destroyed locations station {:x -1 :y -1000} 199)]
    (+ (* (:x last-destroyed) 100) (:y last-destroyed))))
