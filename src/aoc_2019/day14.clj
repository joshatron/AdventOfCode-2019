(ns aoc-2019.day14
  (:require [clojure.string :as str]))

(defn- map-reactants [reactants]
  (into {} (map (fn [r] {(first r) (Integer. (last r))}) (partition 2 (nthrest (reverse reactants) 2)))))

(defn- map-reaction [line]
  (let [parts (remove #{""} (str/split line #" |=>|,"))]
    {(last parts)
     {:quantity (Integer. (first (rest (reverse parts))))
      :from (map-reactants parts)}}))

(defn- map-reactions [input] (into {} (map map-reaction (str/split input #"\n"))))

(defn- remove-resolved [resolved] (remove #(or (= (first %) "ORE") (<= (second %) 0)) resolved))

(defn- subtract-amounts [chemical reaction resolved]
  (assoc (into {} (map #(assoc % 1 (+ (get resolved (first %) 0) (second %))) (:from reaction))) chemical (- (get resolved chemical) (:quantity reaction))))

(defn- replace-chemical [chemical resolved reactions]
  (let [reaction (get reactions chemical)]
    (apply assoc resolved (flatten (into [] (subtract-amounts chemical reaction resolved))))))

(defn- perform-resolution [resolved reactions]
  (let [chosen (first (remove-resolved resolved))]
    (replace-chemical (first chosen) resolved reactions)))

(defn- resolve-reactions [resolved reactions]
  (if (empty? (remove-resolved resolved))
    resolved
    (recur (perform-resolution resolved reactions) reactions)))

(defn puzzle1 [input] (get (resolve-reactions {"FUEL" 1} (map-reactions input)) "ORE"))

(defn- subtract-amounts-perfect [chemical reaction resolved]
  (assoc (into {} (map #(assoc % 1 (+ (get resolved (first %) 0) (* (second %) (/ (get resolved chemical) (:quantity reaction))))) (:from reaction))) chemical 0))

(defn- replace-chemical-perfect [chemical resolved reactions]
  (let [reaction (get reactions chemical)]
    (apply assoc resolved (flatten (into [] (subtract-amounts-perfect chemical reaction resolved))))))

(defn- perform-resolution-perfect [resolved reactions]
  (let [chosen (first (remove-resolved resolved))]
    (replace-chemical-perfect (first chosen) resolved reactions)))

(defn- resolve-reactions-perfect [resolved reactions]
  (if (empty? (remove-resolved resolved))
    resolved
    (recur (perform-resolution-perfect resolved reactions) reactions)))

(defn puzzle2 [input]
  (long (/ 1000000000000 (get (resolve-reactions-perfect {"FUEL" 1} (map-reactions input)) "ORE"))))
