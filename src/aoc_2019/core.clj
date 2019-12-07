(ns aoc-2019.core
  (:require [aoc-2019.day1 :as d1]
            [aoc-2019.day2 :as d2]
            [aoc-2019.day3 :as d3]
            [aoc-2019.day4 :as d4]
            [aoc-2019.day5 :as d5]
            [aoc-2019.day6 :as d6])
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
  (println "Day 4 puzzle 1:" (d4/puzzle1 (slurp "./day4-input.txt")))
  (println "Day 4 puzzle 2:" (d4/puzzle2 (slurp "./day4-input.txt")))
  (println "Day 5 puzzle 1:" (d5/puzzle1 (slurp "./day5-input.txt")))
  (println "Day 5 puzzle 2:" (d5/puzzle2 (slurp "./day5-input.txt")))
  (println "Day 6 puzzle 1:" (d6/puzzle1 (slurp "./day6-input.txt")))
  (println "Day 6 puzzle 2:" (d6/puzzle2 (slurp "./day6-input.txt")))
)
