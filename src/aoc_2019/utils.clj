(ns aoc-2019.utils
  (:require [clojure.string :as str]))

(defn sumInputs
  "Calulates output for each input and sums the result"
  [inputs algorithm]
  (reduce +
          (map #(algorithm (Integer. %))
               (str/split inputs #"\n"))))

