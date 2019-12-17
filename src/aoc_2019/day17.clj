(ns aoc-2019.day17
  (require [aoc-2019.intcomp :as ic]))

(defn- draw-image [input]
  (as-> input i
        (ic/string-to-program i)
        (ic/process-program-till-halt-or-input i [])
        (:output i)
        (map char i)
        (apply str i)
        (str "\n" i)))

(defn- get-image-array [input]
  (as-> input i
        (ic/string-to-program i)
        (ic/process-program-till-halt-or-input i [])
        (:output i)
        (map char i)))

(defn- get-image-2d-array [input]
  (let [array (get-image-array input)
        length (.indexOf array \newline)]
    (partition length (inc length) array)))

(defn- get-char-at-location [image location]
  (nth (nth image (:y location)) (:x location)))

(defn- intersection? [image location]
  (and (= (get-char-at-location image location) \#)
       (= (get-char-at-location image (assoc location :x (dec (:x location)))) \#)
       (= (get-char-at-location image (assoc location :x (inc (:x location)))) \#)
       (= (get-char-at-location image (assoc location :y (dec (:y location)))) \#)
       (= (get-char-at-location image (assoc location :y (inc (:y location)))) \#)))

(defn- image-width [image] (count (first image)))

(defn- image-height [image] (count image))

(defn- edge? [image point]
  (or (= (:x point) 0)
      (= (:x point) (dec (image-width image)))
      (= (:y point) 0)
      (= (:y point) (dec (image-height image)))))

(defn- find-intersections [image]
  (->> (iterate inc 0)
       (take (* (image-width image) (image-height image)))
       (map (fn [n] {:x (mod n (image-width image)) :y (quot n (image-width image))}))
       (remove #(edge? image %))
       (filter #(intersection? image %))))

(defn puzzle1 [input]
  (->> input
       (get-image-2d-array)
       (find-intersections)
       (reduce #(+ %1 (* (:x %2) (:y %2))) 0)))

(defn puzzle2 [input]
  (draw-image input))
