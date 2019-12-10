(ns aoc-2019.intcomp
  (:require [clojure.string :as str]))

(defn- get-address
  [program address relative-base type]
  (case type
    0 (get program address)
    1 address
    2 (+ relative-base (get program address))))

(defn- get-value
  [program address relative-base type]
  (get program (get-address program address relative-base type)))

(defn- process-add
  [program first second result-address]
  (assoc program result-address (+ first second)))

(defn- process-multiply
  [program first second result-address]
  (assoc program result-address (* first second)))

(defn- process-get-input
  [program address input]
  (assoc program address input))

(defn- process-less-than
  [program first second result-address]
    (assoc program result-address (if (< first second) 1 0)))

(defn- process-equal
  [program first second result-address]
  (assoc program result-address (if (= first second) 1 0)))

(defn- parse-op
  [op]
  (let [op-str (str "0000" op)
        op-code (subs op-str (- (count op-str) 2))
        parameter-modes (subs op-str 0 (- (count op-str) 2))]
    {:op (Integer. op-code)
     :first (and (> (count parameter-modes) 0) (Integer. (subs parameter-modes (- (count parameter-modes) 1))))
     :second (and (> (count parameter-modes) 1) (Integer. (subs parameter-modes (- (count parameter-modes) 2) (- (count parameter-modes) 1))))
     :third (and (> (count parameter-modes) 2) (Integer. (subs parameter-modes (- (count parameter-modes) 3) (- (count parameter-modes) 2))))}))

(defn process-program-till-halt-or-input
  [program inputs outputs address relative-base]
  (let [op (parse-op (get program address))]
    (case (:op op)
      ;Add
      1 (recur (process-add program
                            (get-value program (inc address) relative-base (:first op))
                            (get-value program (+ address 2) relative-base (:second op))
                            (get-address program (+ address 3) relative-base (:third op)))
               inputs
               outputs
               (+ address 4)
               relative-base)

      ;Multiply
      2 (recur (process-multiply program
                                 (get-value program (inc address) relative-base (:first op))
                                 (get-value program (+ address 2) relative-base (:second op))
                                 (get-address program (+ address 3) relative-base (:third op)))
               inputs
               outputs
               (+ address 4)
               relative-base)

      ;Take input
      3 (if (seq inputs)
          (recur (process-get-input program
                                    (get-address program (inc address) relative-base (:first op))
                                    (first inputs))
                 (rest inputs)
                 outputs
                 (+ address 2)
                 relative-base)
          {:program program :output outputs :done false :address address :relative-base relative-base})

      ;Add output
      4 (recur program
               inputs
               (conj outputs (get-value program (inc address) relative-base (:first op)))
               (+ address 2)
               relative-base)

      ;Jump if parameter != 0
      5 (recur program
               inputs
               outputs
               (if (not= (get-value program (+ address 1) relative-base (:first op)) 0)
                 (get-value program (+ address 2) relative-base (:second op))
                 (+ address 3))
               relative-base)

      ;Jump if parameter = 0
      6 (recur program
               inputs
               outputs
               (if (= (get-value program (+ address 1) relative-base (:first op)) 0)
                 (get-value program (+ address 2) relative-base (:second op))
                 (+ address 3))
               relative-base)

      ;If A < B, store 1, else store 0
      7 (recur (process-less-than program
                                  (get-value program (inc address) relative-base (:first op))
                                  (get-value program (+ address 2) relative-base (:second op))
                                  (get-address program (+ address 3) relative-base (:third op)))
               inputs
               outputs
               (+ address 4)
               relative-base)

      ;If A = B, store 1, else store 0
      8 (recur (process-equal program
                              (get-value program (inc address) relative-base (:first op))
                              (get-value program (+ address 2) relative-base (:second op))
                              (get-address program (+ address 3) relative-base (:third op)))
               inputs
               outputs
               (+ address 4)
               relative-base)

      ;Move relative base
      9 (recur program
               inputs
               outputs
               (+ address 2)
               (+ relative-base (get-value program (inc address) relative-base (:first op))))

      99 {:program program :output outputs :done true :address address :relative-base relative-base})))

(defn string-to-program
  [str]
  (apply conj (mapv #(Integer. %) (str/split str #",")) (vec (repeat 10000 0))))