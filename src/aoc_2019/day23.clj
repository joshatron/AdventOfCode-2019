(ns aoc-2019.day23
  (:require [aoc-2019.intcomp :as ic]))

(defn- initialize-computer [program n]
  {:program (ic/process-program-till-halt-or-input program [n])
   :packets []})

(defn- initialize-computers [input]
  (as-> (range 50) c
        (mapv (fn [n] [n (initialize-computer (ic/string-to-program input) n)]) c)
        (into (sorted-map) c)
        (assoc c 255 [0,0] 256 1 257 1)
        ))

(defn- run-computer [computer]
  (if (empty? (:packets computer))
    {:program (ic/process-program-till-halt-or-input (:program computer) [-1])
     :packets []}
    {:program (ic/process-program-till-halt-or-input (:program computer) (:packets computer))
     :packets []}))

(defn- update-inputs [computer inputs]
  (assoc computer :packets (apply conj (:packets computer) inputs)))

(defn- send-packet [computers packet]
  (if (= (first packet) 255)
    (assoc computers 255 (rest packet))
    (assoc computers (first packet) (update-inputs (get computers (first packet)) (rest packet)) 254 1)))

(defn- send-packets [computers packets]
  (reduce #(send-packet %1 %2) computers (partition 3 packets)))

(defn- run-one-computer [computers n]
  (let [new-computer (run-computer (get computers n))
        assoced-computers (assoc computers n new-computer)]
    (if (empty? (:output (:program new-computer)))
      assoced-computers
      (send-packets assoced-computers (:output (:program new-computer))))))

(defn- run-computers [computers] (reduce #(run-one-computer %1 %2) computers (range 50)))

(defn- run-network-till-255-address [computers]
  (let [new-computers (run-computers computers)]
    (if (not= (get new-computers 255) [0,0])
      (second (get new-computers 255))
      (recur new-computers))))

(defn puzzle1 [input] (run-network-till-255-address (initialize-computers input)))

(defn- check-for-idle [computers]
  (if (= (get computers 254) 0)
    (assoc computers 0 (update-inputs (get computers 0) (get computers 255))
                     256 (last (get computers 255))
                     257 (- (last (get computers 255)) (get computers 256)))
    (assoc computers 254 0)))

(defn- run-network-till-repeated-deidle [computers]
  (let [new-computers (run-computers computers)
        deidled-computers (check-for-idle new-computers)]
    (if (= (get new-computers 257) 0)
      (get new-computers 256)
      (recur deidled-computers))))

(defn puzzle2 [input] (run-network-till-repeated-deidle (initialize-computers input)))
