(ns aoc-2019.day16)

(defn- repeat-value [value n] (take n (repeat value)))

(defn- create-pattern [n]
  (->> [(repeat-value 0 n) (repeat-value 1 n) (repeat-value 0 n) (repeat-value -1 n)]
       (flatten)
       (cycle)
       (rest)))

(defn- parse-input [input]
  (->> input
       (map str)
       (map #(Integer. %))
       (vec)))

(defn- map-to-num [data pattern total]
  (if (empty? data)
    (mod (Math/abs total) 10)
    (recur (rest data) (rest pattern) (+ total (* (first data) (first pattern))))))

(defn- run-phase [data]
  (map #(map-to-num data (create-pattern %) 0) (take (count data) (iterate inc 1))))

(defn- run-phases [count data]
  (if (= count 0)
    data
    (recur (dec count) (run-phase data))))

(defn puzzle1 [input]
  (->> (parse-input input)
       (run-phases 100)
       (take 8)
       (apply str)))

(defn puzzle2 [input])
