(ns aoc-2019.day16)

(defn- repeat-value [value n] (take n (repeat value)))

(defn- create-pattern [n length]
  (->> [(repeat-value 0 n) (repeat-value 1 n) (repeat-value 0 n) (repeat-value -1 n)]
       (flatten)
       (cycle)
       (rest)
       (take length)
       (vec)))

(defn- initialize-patterns [n]
  (mapv #(create-pattern % n)(take n (iterate inc 1))))

(defn- parse-input [input]
  (->> input
       (map str)
       (map #(Integer. %))
       (vec)))

(defn- map-to-num [data pattern total]
  (if (empty? data)
    (mod (Math/abs total) 10)
    (recur (rest data) (rest pattern) (+ total (* (first data) (first pattern))))))

(defn- run-phase [data patterns]
  (map #(map-to-num data (get patterns %) 0) (take (count data) (iterate inc 0))))

(defn- run-phases [count patterns data]
  (if (= count 0)
    data
    (recur (dec count) patterns (run-phase data patterns))))

(defn puzzle1 [input]
  (->> (parse-input input)
       (run-phases 100 (initialize-patterns (count input)))
       (take 8)
       (apply str)))

(defn- get-shifted-repeated-input [input]
  (as-> input d
       (parse-input d)
       (cycle d)
       (take (* (count input) 10000) d)
       (nthrest d (Integer. (subs input 0 7)))))

(defn- do-simple-phase [data sum new-list]
  (if (empty? data)
    new-list
    (recur (rest data) (mod (+ sum (first data)) 10) (conj new-list (mod (+ sum (first data)) 10)))))

(defn- do-simple-phases [repeat data]
  (if (= repeat 0)
    data
    (recur (dec repeat) (do-simple-phase data 0 []))))

(defn puzzle2 [input]
  (->> (get-shifted-repeated-input input)
       (reverse)
       (do-simple-phases 100)
       (reverse)
       (take 8)
       (apply str)))
