(ns aoc-2019.day5
  (:require [aoc-2019.intcomp :as ic]))

(defn puzzle1
  [input]
  (last (ic/getProgramOutput (ic/stringToProgram input) [1] [] 0)))

(defn puzzle2
  [input]
  (first (ic/getProgramOutput (ic/stringToProgram input) [5] [] 0)))
