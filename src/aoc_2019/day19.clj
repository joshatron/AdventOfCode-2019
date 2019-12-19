(ns aoc-2019.day19
  (:require [aoc-2019.intcomp :as ic]))

(defn- create-points [width height]
  (map (fn [n] {:x (mod n width) :y (quot n height)}) (take (* width height) (iterate inc 0))))

(defn- print-line [points]
  (cond
    (empty? points) (println)
    (= (first points) 0) (do (print ".") (recur (rest points)))
    :else (do (print "#") (recur (rest points)))))

(defn- print-locations
  ([width points] (print-locations (partition width points) width points))
  ([sections width original]
   (if (empty? sections)
     original
     (do
       (print-line (first sections))
       (recur (rest sections) width original)))))

(defn puzzle1 [input]
  (let [program (ic/string-to-program input)]
    (->> (create-points 50 50)
         (map #(ic/process-program-till-halt-or-input program [(:x %) (:y %)]))
         (map :output)
         (map first)
         (print-locations 50) ; Uncomment for visualization
         (filter #(= % 1))
         (count)
         )))

(defn- find-offset [y program width]
  (if (= (first (:output (ic/process-program-till-halt-or-input program [(- y width) y]))) 1)
    width
    (recur y program (inc width))))

(defn- ship-fits?
  ([y program] (ship-fits? y program 0 0 (find-offset y program 0)))
  ([y program width height offset]
   (let [output (first (:output (ic/process-program-till-halt-or-input program [(long (- y width offset)) (long (+ y height))])))]
     (cond
       (= output 0) false
       (and (= width 99) (= height 99)) {:x (- y width offset) :y y}
       (= width 99) (recur y program width (inc height) offset)
       :else (recur y program (inc width) height offset)))))

(defn puzzle2 [input]
  (let [program (ic/string-to-program input)
        upper-range (:y (some #(ship-fits? % program) (iterate #(+ % 100) 10)))
        loc (some #(ship-fits? % program) (iterate inc (- upper-range 100)))]
    (+ (* (:x loc) 10000) (:y loc))))
