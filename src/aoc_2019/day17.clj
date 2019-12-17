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

(defn- image-width [image] (count (first image)))

(defn- image-height [image] (count image))

(defn- off-map? [image point]
  (or (< (:x point) 0)
      (> (:x point) (dec (image-width image)))
      (< (:y point) 0)
      (> (:y point) (dec (image-height image)))))

(defn- get-char-at-location [image location]
  (if (off-map? image location)
    \.
    (nth (nth image (:y location)) (:x location))))

(defn- intersection? [image location]
  (and (= (get-char-at-location image location) \#)
       (= (get-char-at-location image (assoc location :x (dec (:x location)))) \#)
       (= (get-char-at-location image (assoc location :x (inc (:x location)))) \#)
       (= (get-char-at-location image (assoc location :y (dec (:y location)))) \#)
       (= (get-char-at-location image (assoc location :y (inc (:y location)))) \#)))

(defn- find-intersections [image]
  (->> (iterate inc 0)
       (take (* (image-width image) (image-height image)))
       (map (fn [n] {:x (mod n (image-width image)) :y (quot n (image-width image))}))
       (filter #(intersection? image %))))

(defn puzzle1 [input]
  (->> input
       (get-image-2d-array)
       (find-intersections)
       (reduce #(+ %1 (* (:x %2) (:y %2))) 0)))

(defn- get-initial-state
  ([image] (get-initial-state image 0))
  ([image n]
   (if (some #{\^} (first image))
     {:dir 0
      :x (.indexOf (first image) \^)
      :y n}
     (recur (rest image) (inc n)))))

;   0
; 3   1
;   2
(defn- move-one [state]
  (case (:dir state)
    0 (assoc state :y (dec (:y state)))
    1 (assoc state :x (inc (:x state)))
    2 (assoc state :y (inc (:y state)))
    3 (assoc state :x (dec (:x state)))))

(defn- move-n [state n]
  (if (= n 0)
    state
    (recur (move-one state) (dec n))))

(defn- turn-left [state]
  (if (= (:dir state) 0)
    (assoc state :dir 3)
    (assoc state :dir (dec (:dir state)))))

(defn- turn-right [state]
  (if (= (:dir state) 3)
    (assoc state :dir 0)
    (assoc state :dir (inc (:dir state)))))

(defn- scaffolding-in-front? [image state]
  (= (get-char-at-location image (move-one state)) \#))

(defn- distance-forward
  ([image state] (distance-forward image state 0))
  ([image state n]
   (if (scaffolding-in-front? image state)
     (recur image (move-one state) (inc n))
     n)))

(defn- get-movement-instructions
  ([image] (get-movement-instructions image (get-initial-state image) []))
  ([image state steps]
   (cond
     (scaffolding-in-front? image state) (recur image (move-n state (distance-forward image state)) (conj steps (distance-forward image state)))
     (scaffolding-in-front? image (turn-left state)) (recur image (turn-left state) (conj steps "L"))
     (scaffolding-in-front? image (turn-right state)) (recur image (turn-right state) (conj steps "R"))
     :else steps)))

(defn- get-commands []
  ; Determined manually through looking at the steps from get-movement-instructions
  (->> "A,B,A,C,A,B,C,B,C,A\nL,12,R,4,R,4,L,6\nL,12,R,4,R,4,R,12\nL,10,L,6,R,4\nn\n"
       (map char)
       (map int)))

(defn puzzle2 [input]
  (-> input
      (ic/string-to-program)
      (assoc 0 2)
      (ic/process-program-till-halt-or-input (get-commands))
      (:output)
      (last)))
