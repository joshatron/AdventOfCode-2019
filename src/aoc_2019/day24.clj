(ns aoc-2019.day24
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(defn- to-bug [string] (if (= string \#) 1 0))

(defn- get-flatland [input]
  (->> (str/split input #"\n")
       (map #(map to-bug %))
       (flatten)
       (into [])))

(defn- get-up [flatland loc] (if (< loc 5) 0 (get flatland (- loc 5))))
(defn- get-down [flatland loc] (if (> loc 19) 0 (get flatland (+ loc 5))))
(defn- get-left [flatland loc] (if (= (mod loc 5) 0) 0 (get flatland (dec loc))))
(defn- get-right [flatland loc] (if (= (mod loc 5) 4) 0 (get flatland (inc loc))))


(defn- get-surrounding [flatland loc]
  (+ (get-up flatland loc) (get-down flatland loc) (get-left flatland loc) (get-right flatland loc)))

(defn- update-loc [current surrounding]
  (cond
    (and (= current 1) (= surrounding 1)) 1
    (= current 1) 0
    (<= 1 surrounding 2) 1
    :else 0))

(defn- update-world [flatland]
  (->> (range 25)
       (mapv #(update-loc (get flatland %) (get-surrounding flatland %)))))

(defn- get-expt-at-point [flatland point]
  (if (= (get flatland point) 1)
    (math/expt 2 point)
    0))

(defn- get-biodiversity-rating [flatland]
  (->> (range 25)
       (map #(get-expt-at-point flatland %))
       (reduce +)))

(defn- run-till-repeat [flatland ratings]
  (let [bio-rating (get-biodiversity-rating flatland)]
    (if (contains? ratings bio-rating)
      bio-rating
      (recur (update-world flatland) (conj ratings bio-rating)))))

(defn puzzle1 [input] (run-till-repeat (get-flatland input) (hash-set)))

(defn puzzle2 [input])
