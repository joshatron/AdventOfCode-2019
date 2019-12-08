(ns aoc-2019.day7
  (:require [aoc-2019.intcomp :as ic]))

(defn- getPermutationsRecur
  [start range existingSeq current allSeqs]
  (cond
    (= current (+ range start)) allSeqs
    (some #(= current %) existingSeq) (recur start range existingSeq (inc current) allSeqs)
    (= (count existingSeq) (dec range)) (recur start range existingSeq (inc current) (conj allSeqs (conj existingSeq current)))
    :else (apply conj (getPermutationsRecur start range (conj existingSeq current) start allSeqs) (getPermutationsRecur start range existingSeq (inc current) allSeqs))
    ))

(defn- getPermutations
  [start range]
  (getPermutationsRecur start range [] start []))

(defn- runProgramSequence
  [program sequence input]
  (if (empty? sequence)
    input
    (recur program (rest sequence) (first (ic/getProgramOutput program [(first sequence) input] [] 0)))))

(defn puzzle1
  [input]
  (let [program (ic/stringToProgram input)
        permutations (getPermutations 0 5)]
    (apply max (map #(runProgramSequence program % 0) permutations))))

(defn- initializeAmps
  [program config amps input]
  (if (empty? config)
    amps
    (let [amp (ic/processProgramTillHaltOrInput program [(first config) input] [] 0)]
      (initializeAmps program (rest config) (conj amps amp) (get (:output amp) 0)))))

(defn- runPassOfAmps
  [amps index]
  (cond
    (= (count amps) index) amps
    (= index 0) (recur (assoc amps index (ic/processProgramTillHaltOrInput (:program (get amps index)) (:output (last amps)) [] (:address (get amps index)))) (inc index))
    :else (recur (assoc amps index (ic/processProgramTillHaltOrInput (:program (get amps index)) (:output (get amps (dec index))) [] (:address (get amps index)))) (inc index))
    ))

(defn- runAmps
  [program amps]
  (if (:done (last amps))
    (get (:output (last amps)) 0)
    (recur program (runPassOfAmps amps 0))))

(defn puzzle2
  [input]
  (let [configs (getPermutations 5 5)
        program (ic/stringToProgram input)]
    (apply max (map #(runAmps program (initializeAmps program % [] 0)) configs))))

