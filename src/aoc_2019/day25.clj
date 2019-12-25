(ns aoc-2019.day25
  (:require [aoc-2019.intcomp :as ic]))

(defn- output-to-str [output]
  (->> output
       (map char)
       (apply str)))

(defn- command-to-input [command]
  (->> command
       (map char)
       (map int)))

(defn- command-robot [program command]
  (let [new-program (ic/process-program-till-halt-or-input program command)]
    (println (output-to-str (:output new-program)))
    (if (:done new-program)
      new-program
      (recur new-program (command-to-input (str (read-line) "\n"))))))

; You need cake, jam, easter egg, and asterisk to make it through
(defn puzzle1 [input] (command-robot (ic/string-to-program input) []))

(defn puzzle2 [input] "Contrats on finishing!")
