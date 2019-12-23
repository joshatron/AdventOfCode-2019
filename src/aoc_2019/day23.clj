(ns aoc-2019.day23
  (:require [aoc-2019.intcomp :as ic]))

(defn- initialize-computer [program n]
  {:program (ic/process-program-till-halt-or-input program [n])
   :packets []})

(defn- initialize-computers [input]
  (->> (range 50)
       (mapv #(initialize-computer (ic/string-to-program input) %))))

(defn- run-computer [computer]
  (if (empty? (:packets computer))
    {:program (ic/process-program-till-halt-or-input (:program computer) [-1])
     :packets []}
    {:program (ic/process-program-till-halt-or-input (:program computer) (:packets computer))
     :packets []}))

(defn- update-inputs [computer inputs]
  (assoc computer :packets (apply conj (:packets computer) inputs)))

(defn- send-packet [computers packet]
  (cond
    (number? computers) computers
    (= (first packet) 255) (last packet)
    :else (assoc computers (first packet) (update-inputs (get computers (first packet)) (rest packet)))))

(defn- send-packets [computers packets]
  (reduce #(send-packet %1 %2) computers (partition 3 packets)))

(defn- run-one-computer [computers n]
  (if (number? computers)
    computers
    (let [new-computer (run-computer (get computers n))
          assoced-computers (assoc computers n new-computer)]
      (if (empty? (:output (:program new-computer)))
        assoced-computers
        (send-packets assoced-computers (:output (:program new-computer)))))))

(defn- run-network-till-255-address [computers]
  (let [new-computers (reduce #(run-one-computer %1 %2) computers (range 50))]
    (if (number? new-computers)
      new-computers
      (recur new-computers))))

(defn puzzle1 [input] (run-network-till-255-address (initialize-computers input)))

(defn puzzle2 [input])
