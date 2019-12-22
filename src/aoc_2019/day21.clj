(ns aoc-2019.day21
  (:require [aoc-2019.intcomp :as ic]))

(defn- translate-springscript [script]
  (->> script
       (map char)
       (map int)))

(defn- draw-image [output]
  (->> output
       (map char)
       (apply str)
       (str "\n")))

(defn- run-failing-script [program script]
  (->> (ic/process-program-till-halt-or-input program (translate-springscript script))
       (:output)
       (draw-image)))

(defn- run-passing-script [program script]
  (->> (ic/process-program-till-halt-or-input program (translate-springscript script))
       (:output)
       (last)))

(defn- get-p1-script []
  (str "OR A T\n"
       "AND B T\n"
       "AND C T\n"
       "NOT T T\n"
       "OR D J\n"
       "AND T J\n"
       "WALK\n"))

(defn puzzle1 [input] (run-passing-script (ic/string-to-program input) (get-p1-script)))

(defn- get-p2-script []
  (str "OR C T\n"
       "NOT T T\n"
       "AND D T\n"
       "AND H T\n"
       "OR A J\n"
       "AND B J\n"
       "NOT J J\n"
       "AND D J\n"
       "OR T J\n"
       "RUN\n"))

(defn puzzle2 [input] (run-passing-script (ic/string-to-program input) (get-p2-script)))
