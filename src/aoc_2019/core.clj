(ns aoc-2019.core
  (:require [aoc-2019.day1 :as d1]
            [aoc-2019.day2 :as d2]
            [aoc-2019.day3 :as d3]
            [aoc-2019.day4 :as d4]
            [aoc-2019.day5 :as d5]
            [aoc-2019.day6 :as d6]
            [aoc-2019.day7 :as d7]
            [aoc-2019.day8 :as d8]
            [aoc-2019.day9 :as d9]
            [aoc-2019.day10 :as d10]
            [aoc-2019.day11 :as d11]
            [aoc-2019.day12 :as d12]
            [aoc-2019.day13 :as d13]
            [aoc-2019.day14 :as d14]
            [aoc-2019.day15 :as d15]
            [aoc-2019.day16 :as d16]
            [aoc-2019.day17 :as d17]
            [aoc-2019.day18 :as d18]
            [aoc-2019.day19 :as d19]
            [aoc-2019.day20 :as d20]
            [aoc-2019.day21 :as d21]
            [aoc-2019.day22 :as d22]
            [aoc-2019.day23 :as d23]
            [aoc-2019.day24 :as d24]
            [aoc-2019.day25 :as d25])
  (:gen-class))

(defn -main
  [& args]
  ;(println "Day 1 puzzle 1:" (d1/puzzle1 (slurp "./day1-input.txt")))
  ;(println "Day 1 puzzle 2:" (d1/puzzle2 (slurp "./day1-input.txt")))
  ;(println "Day 2 puzzle 1:" (d2/puzzle1 (slurp "./day2-input.txt")))
  ;(println "Day 2 puzzle 2:" (d2/puzzle2 (slurp "./day2-input.txt")))
  ;(println "Day 3 puzzle 1:" (d3/puzzle1 (slurp "./day3-input.txt")))
  ;(println "Day 3 puzzle 2:" (d3/puzzle2 (slurp "./day3-input.txt")))
  ;(println "Day 4 puzzle 1:" (d4/puzzle1 (slurp "./day4-input.txt")))
  ;(println "Day 4 puzzle 2:" (d4/puzzle2 (slurp "./day4-input.txt")))
  ;(println "Day 5 puzzle 1:" (d5/puzzle1 (slurp "./day5-input.txt")))
  ;(println "Day 5 puzzle 2:" (d5/puzzle2 (slurp "./day5-input.txt")))
  ;(println "Day 6 puzzle 1:" (d6/puzzle1 (slurp "./day6-input.txt")))
  ;(println "Day 6 puzzle 2:" (d6/puzzle2 (slurp "./day6-input.txt")))
  ;(println "Day 7 puzzle 1:" (d7/puzzle1 (slurp "./day7-input.txt")))
  ;(println "Day 7 puzzle 2:" (d7/puzzle2 (slurp "./day7-input.txt")))
  ;(println "Day 8 puzzle 1:" (d8/puzzle1 (slurp "./day8-input.txt")))
  ;(println "Day 8 puzzle 2:" (d8/puzzle2 (slurp "./day8-input.txt")))
  ;(println "Day 9 puzzle 1:" (d9/puzzle1 (slurp "./day9-input.txt")))
  ;(println "Day 9 puzzle 2:" (d9/puzzle2 (slurp "./day9-input.txt")))
  ;(println "Day 10 puzzle 1:" (d10/puzzle1 (slurp "./day10-input.txt")))
  ;(println "Day 10 puzzle 2:" (d10/puzzle2 (slurp "./day10-input.txt")))
  ;(println "Day 11 puzzle 1:" (d11/puzzle1 (slurp "./day11-input.txt")))
  ;(println "Day 11 puzzle 2:" (d11/puzzle2 (slurp "./day11-input.txt")))
  ;(println "Day 12 puzzle 1:" (d12/puzzle1 (slurp "./day12-input.txt")))
  ;(println "Day 12 puzzle 2:" (d12/puzzle2 (slurp "./day12-input.txt")))
  ;(println "Day 13 puzzle 1:" (d13/puzzle1 (slurp "./day13-input.txt")))
  ;(println "Day 13 puzzle 2:" (d13/puzzle2 (slurp "./day13-input.txt")))
  ;(println "Day 14 puzzle 1:" (d14/puzzle1 (slurp "./day14-input.txt")))
  ;(println "Day 14 puzzle 2:" (d14/puzzle2 (slurp "./day14-input.txt")))
  ;(println "Day 15 puzzle 1:" (d15/puzzle1 (slurp "./day15-input.txt")))
  ;(println "Day 15 puzzle 2:" (d15/puzzle2 (slurp "./day15-input.txt")))
  ;(println "Day 16 puzzle 1:" (d16/puzzle1 (slurp "./day16-input.txt")))
  ;(println "Day 16 puzzle 2:" (d16/puzzle2 (slurp "./day16-input.txt")))
  ;(println "Day 17 puzzle 1:" (d17/puzzle1 (slurp "./day17-input.txt")))
  ;(println "Day 17 puzzle 2:" (d17/puzzle2 (slurp "./day17-input.txt")))
  (println "Day 18 puzzle 1:" (d18/puzzle1 (slurp "./day18-input.txt")))
  ;(println "Day 18 puzzle 2:" (d18/puzzle2 (slurp "./day18-input.txt")))
  ;(println "Day 19 puzzle 1:" (d19/puzzle1 (slurp "./day19-input.txt")))
  ;(println "Day 19 puzzle 2:" (d19/puzzle2 (slurp "./day19-input.txt")))
  ;(println "Day 20 puzzle 1:" (d20/puzzle1 (slurp "./day20-input.txt")))
  ;(println "Day 20 puzzle 2:" (d20/puzzle2 (slurp "./day20-input.txt")))
  ;(println "Day 21 puzzle 1:" (d21/puzzle1 (slurp "./day21-input.txt")))
  ;(println "Day 21 puzzle 2:" (d21/puzzle2 (slurp "./day21-input.txt")))
  ;(println "Day 22 puzzle 1:" (d22/puzzle1 (slurp "./day22-input.txt")))
  ;(println "Day 22 puzzle 2:" (d22/puzzle2 (slurp "./day22-input.txt")))
  (println "Day 23 puzzle 1:" (d23/puzzle1 (slurp "./day23-input.txt")))
  (println "Day 23 puzzle 2:" (d23/puzzle2 (slurp "./day23-input.txt")))
  (println "Day 24 puzzle 1:" (d24/puzzle1 (slurp "./day24-input.txt")))
  (println "Day 24 puzzle 2:" (d24/puzzle2 (slurp "./day24-input.txt")))
  (println "Day 25 puzzle 1:" (d25/puzzle1 (slurp "./day25-input.txt")))
  (println "Day 25 puzzle 2:" (d25/puzzle2 (slurp "./day25-input.txt"))))
