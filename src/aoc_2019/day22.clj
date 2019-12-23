(ns aoc-2019.day22
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(defn- initialize-deck [size]
  (->> (iterate inc 0)
       (take size)
       (into [])))

(defn- reverse-deck [deck]
  (->> deck
       (rseq)
       (into [])))

(defn- perform-cut [deck cut-point]
  (into [] (concat (subvec deck cut-point) (subvec deck 0 cut-point))))

(defn- cut-deck [deck cut-point]
  (if (> cut-point 0)
    (perform-cut deck cut-point)
    (perform-cut deck (+ (count deck) cut-point))))

(defn- shuffle-deck
  ([deck increment] (shuffle-deck deck (vec (take (count deck) (repeat 0))) 0 0 increment))
  ([original-deck new-deck current-original current-new increment]
   (if (= (count original-deck) current-original)
     new-deck
     (recur original-deck
            (assoc new-deck current-new (get original-deck current-original))
            (inc current-original)
            (mod (+ current-new increment) (count original-deck))
            increment))))

(defn- get-last-num [string]
  (-> string
      (str/split #" ")
      (last)
      (Integer.)))

(defn- perform-operation [deck operation]
  (cond
    (str/starts-with? operation "deal into new stack") (reverse-deck deck)
    (str/starts-with? operation "cut") (cut-deck deck (get-last-num operation))
    (str/starts-with? operation "deal with increment") (shuffle-deck deck (get-last-num operation))))

(defn- perform-operations [deck operations]
  (if (empty? operations)
    deck
    (recur (perform-operation deck (first operations)) (rest operations))))

(defn puzzle1 [input] (.indexOf (perform-operations (initialize-deck 10007) (str/split input #"\n")) 2019))

; Taken from https://rosettacode.org/wiki/Modular_inverse#Clojure
(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn mul_inv
  " Get inverse using extended gcd.  Extended GCD returns
    gcd followed by bezout coefficients. We want the 1st coefficients
   (i.e. second of extend-gcd result).  We compute mod base so result
    is between 0..(base-1) "
  [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (extended-gcd a b)]
    (if (= (first egcd) 1)
      (mod (second egcd) b)
      (str "No inverse since gcd is: " (first egcd)))))

(defn- reverse-reverse-deck [deck-size num] (-' deck-size 1 num))

(defn- reverse-cut-deck [deck-size num cut-size] (mod (+' num cut-size deck-size) deck-size))

(defn- reverse-shuffle-deck [deck-size num inc-size] (mod (*' (mul_inv inc-size deck-size) num) deck-size))

(defn- reverse-operation [deck-size num operation]
  (cond
    (str/starts-with? operation "deal into new stack") (reverse-reverse-deck deck-size num)
    (str/starts-with? operation "cut") (reverse-cut-deck deck-size num (get-last-num operation))
    (str/starts-with? operation "deal with increment") (reverse-shuffle-deck deck-size num (get-last-num operation))))

(defn- reverse-operations [deck-size num operations]
  (if (empty? operations)
    num
    (recur deck-size (reverse-operation deck-size num (first operations)) (rest operations))))

(defn- apply-operations-many-times [deck-size num-to-check times-done operations]
  (let [x num-to-check
        y (reverse-operations deck-size num-to-check operations)
        z (reverse-operations deck-size y operations)
        a (mod (*' (-' y z) (mul_inv (-' x y) deck-size)) deck-size)
        b (mod (-' y (*' a x)) deck-size)]
    (mod (+' (*' (.modPow (BigInteger/valueOf a) (BigInteger/valueOf times-done) (BigInteger/valueOf deck-size)) x) (*' (dec' (.modPow (BigInteger/valueOf a) (BigInteger/valueOf times-done) (BigInteger/valueOf deck-size))) (mul_inv (dec' a) deck-size) b)) deck-size)))

(defn puzzle2 [input] (apply-operations-many-times 119315717514047 2020 101741582076661 (reverse (str/split input #"\n"))))
