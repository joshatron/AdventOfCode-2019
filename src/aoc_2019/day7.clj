(ns aoc-2019.day7
  (:require [aoc-2019.intcomp :as ic]))

(defn- getPermutationsRecur
  [max existingSeq current allSeqs]
  (cond
    (= current max) allSeqs
    (some #(= current %) existingSeq) (recur max existingSeq (inc current) allSeqs)
    (= (count existingSeq) (dec max)) (recur max existingSeq (inc current) (conj allSeqs (conj existingSeq current)))
    :else (apply conj (getPermutationsRecur max (conj existingSeq current) 0 allSeqs) (getPermutationsRecur max existingSeq (inc current) allSeqs))
    ))

(defn- getPermutations
  [max]
  (getPermutationsRecur max [] 0 []))

(defn- runProgramSequence
  [program sequence input]
  (if (empty? sequence)
    input
    (recur program (rest sequence) (first (ic/getProgramOutput program [(first sequence) input] [] 0)))))

(defn puzzle1
  [input]
  (let [program (ic/stringToProgram input)
        permutations (getPermutations 5)]
    (apply max (map #(runProgramSequence program % 0) (getPermutations 5)))))

(defn puzzle2
  [input])