(ns aoc-2019.day13
  (:require [aoc-2019.intcomp :as ic]))

(defn- draw-outputs
  ([outputs] (draw-outputs outputs []))
  ([outputs tiles]
  (if (empty? outputs)
    tiles
    (draw-outputs (nthrest outputs 3) (conj tiles {:x (first outputs) :y (nth outputs 1) :id (nth outputs 2)})))))

(defn puzzle1 [input]
  (as-> input v
        (ic/string-to-program v)
        (ic/process-program-till-halt-or-input v [])
        (:output v)
        (draw-outputs v)
        (filter #(= (:id %) 2) v)
        (count v)))
