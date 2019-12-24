(ns aoc-2019.day24
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(defn- to-bug [character] (if (= character \#) 1 0))

(defn- get-flatland [input]
  (->> (str/split input #"\n")
       (map #(map to-bug %))
       (flatten)
       (into [])))

(defn- get-up [flatland loc] (if (< loc 5) 0 (get flatland (- loc 5))))
(defn- get-down [flatland loc] (if (> loc 19) 0 (get flatland (+ loc 5))))
(defn- get-left [flatland loc] (if (= (mod loc 5) 0) 0 (get flatland (dec loc))))
(defn- get-right [flatland loc] (if (= (mod loc 5) 4) 0 (get flatland (inc loc))))


(defn- get-surrounding [flatland loc]
  (+ (get-up flatland loc) (get-down flatland loc) (get-left flatland loc) (get-right flatland loc)))

(defn- update-loc [current surrounding]
  (cond
    (and (= current 1) (= surrounding 1)) 1
    (= current 1) 0
    (<= 1 surrounding 2) 1
    :else 0))

(defn- update-world [flatland]
  (->> (range 25)
       (mapv #(update-loc (get flatland %) (get-surrounding flatland %)))))

(defn- get-expt-at-point [flatland point]
  (if (= (get flatland point) 1)
    (math/expt 2 point)
    0))

(defn- get-biodiversity-rating [flatland]
  (->> (range 25)
       (map #(get-expt-at-point flatland %))
       (reduce +)))

(defn- run-till-repeat [flatland ratings]
  (let [bio-rating (get-biodiversity-rating flatland)]
    (if (contains? ratings bio-rating)
      bio-rating
      (recur (update-world flatland) (conj ratings bio-rating)))))

(defn puzzle1 [input] (run-till-repeat (get-flatland input) (hash-set)))

(defn- get-initial-world [input]
  (let [base-input (str/replace input #"\n" "")]
    (->> (range 25)
         (filter #(= (subs base-input % (inc %)) "#"))
         (map (fn [n] {:x (mod n 5) :y (quot n 5) :z 0}))
         (into (hash-set)))))

(defn- replace-up [loc]
  (if (< (:y loc) 0)
    {:x 2 :y 1 :z (dec (:z loc))}
    loc))

(defn- replace-down [loc]
  (if (> (:y loc) 4)
    {:x 2 :y 3 :z (dec (:z loc))}
    loc))

(defn- replace-left [loc]
  (if (< (:x loc) 0)
    {:x 1 :y 2 :z (dec (:z loc))}
    loc))

(defn- replace-right [loc]
  (if (> (:x loc) 4)
    {:x 3 :y 2 :z (dec (:z loc))}
    loc))

(defn- inner-above [level]
  [{:x 0 :y 0 :z (inc level)}
   {:x 1 :y 0 :z (inc level)}
   {:x 2 :y 0 :z (inc level)}
   {:x 3 :y 0 :z (inc level)}
   {:x 4 :y 0 :z (inc level)}])

(defn- inner-below [level]
  [{:x 0 :y 4 :z (inc level)}
   {:x 1 :y 4 :z (inc level)}
   {:x 2 :y 4 :z (inc level)}
   {:x 3 :y 4 :z (inc level)}
   {:x 4 :y 4 :z (inc level)}])

(defn- inner-left [level]
  [{:x 0 :y 0 :z (inc level)}
   {:x 0 :y 1 :z (inc level)}
   {:x 0 :y 2 :z (inc level)}
   {:x 0 :y 3 :z (inc level)}
   {:x 0 :y 4 :z (inc level)}])

(defn- inner-right [level]
  [{:x 4 :y 0 :z (inc level)}
   {:x 4 :y 1 :z (inc level)}
   {:x 4 :y 2 :z (inc level)}
   {:x 4 :y 3 :z (inc level)}
   {:x 4 :y 4 :z (inc level)}])

(defn- replace-inner [loc original]
  (if (and (= (:x loc) 2) (= (:y loc) 2))
    (cond
      (= (:x original) 1) (inner-left (:z loc))
      (= (:x original) 3) (inner-right (:z loc))
      (= (:y original) 1) (inner-above (:z loc))
      (= (:y original) 3) (inner-below (:z loc)))
    [loc]))

(defn- find-surrounding [loc]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map #(assoc loc :x (+ (:x loc) (first %)) :y (+ (:y loc) (second %))))
       (map #(replace-inner % loc))
       (flatten)
       (map replace-up)
       (map replace-down)
       (map replace-left)
       (map replace-right)))

(defn- get-num-surrounding [world loc]
  (->> loc
       (find-surrounding)
       (filter #(contains? world %))
       (count)))

(defn- kill-bugs [world] (into (hash-set) (filter #(= (get-num-surrounding world %) 1) world)))

(defn- spawn-bugs [world]
  (->> world
       (map find-surrounding)
       (flatten)
       (into (hash-set))
       (remove world)
       (filter #(<= 1 (get-num-surrounding world %) 2))
       (into (hash-set))))

(defn- merge-worlds [world1 world2]
  (apply conj world1 world2))

(defn- update-world-for-one-minute [world]
  (merge-worlds (kill-bugs world) (spawn-bugs world)))

(defn- update-world-for-n-minutes [world minutes]
  (if (= minutes 0)
    world
    (recur (update-world-for-one-minute world) (dec minutes))))

(defn puzzle2 [input]
  (count (update-world-for-n-minutes (get-initial-world input) 200)))
