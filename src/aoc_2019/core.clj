(ns aoc-2019.core
  (:require [aoc-2019.day1 :as d1p1])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print "Day 1 puzzle 1: ")
  (println (d1p1/puzzle1 (slurp "./day1-input.txt")))
  (print "Day 1 puzzle 2: ")
  (println (d1p1/puzzle2 (slurp "./day1-input.txt")))
  )
