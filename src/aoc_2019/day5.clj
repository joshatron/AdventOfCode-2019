(ns aoc-2019.day5
  (:require [aoc-2019.intcomp :as ic]))

(defn puzzle1
  [input]
  (last (:output (ic/process-program-till-halt-or-input (ic/string-to-program input) [1] [] 0 0))))

(defn puzzle2
  [input]
  (first (:output (ic/process-program-till-halt-or-input (ic/string-to-program input) [5] [] 0 0))))
