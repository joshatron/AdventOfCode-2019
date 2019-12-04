(ns aoc-2019.core
  (:require [aoc-2019.day1 :as d1])
  (:require [aoc-2019.day2 :as d2])
  (:require [aoc-2019.day3 :as d3])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Day 1 puzzle 1:" (d1/puzzle1 (slurp "./day1-input.txt")))
  (println "Day 1 puzzle 2:" (d1/puzzle2 (slurp "./day1-input.txt")))
  (println "Day 2 puzzle 1:" (d2/puzzle1 (slurp "./day2-input.txt")))
  (println "Day 2 puzzle 2:" (d2/puzzle2 (slurp "./day2-input.txt")))
  (println "Day 3 puzzle 1:" (d3/puzzle1 (slurp "./day3-input.txt")))
  (println "Day 3 puzzle 2:" (d3/puzzle2 (slurp "./day3-input.txt")))
)
