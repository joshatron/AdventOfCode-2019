(ns aoc-2019.day15
  (:require [aoc-2019.intcomp :as ic]))

(defn- move-loc [loc dir]
  (case dir
    1 {:x (:x loc) :y (inc (:y loc))}
    2 {:x (:x loc) :y (dec (:y loc))}
    3 {:x (dec (:x loc)) :y (:y loc)}
    4 {:x (inc (:x loc)) :y (:y loc)}))

(defn- try-move [state dir visited]
  (let [moved-program (ic/process-program-till-halt-or-input (:program state) [dir])]
    (cond
      (= (first (:output moved-program)) 0) nil
      (some #{(move-loc (:loc state) dir)} visited) nil
      :else {:program moved-program :loc (move-loc (:loc state) dir)})))

(defn- get-possible [state visited] (remove nil? (map #(try-move state % visited) [1 2 3 4])))

(defn- final-spot [state] (= (first (:output (:program state))) 2))

(defn- depth-first-find-location
  ([input] (depth-first-find-location {:program (ic/string-to-program input) :loc {:x 0 :y 0}} []))
  ([state visited]
   (let [next (get-possible state visited)]
    (cond
      (empty? next) nil
      (some final-spot next) (first (filter final-spot next))
      :else (some #(depth-first-find-location % (conj visited (:loc state))) next)
      ))))

(defn- get-smallest [list]
  (reduce #(if (< (:f %1) (:f %2)) %1 %2) list))

(defn- get-manhattan-distance [p1 p2]
  (+ (Math/abs (- (:x p1) (:x p2))) (Math/abs (- (:y p1) (:y p2)))))

(defn- set-vars [state parent destination]
  (assoc state :g (inc (:g parent)) :f (+ (get-manhattan-distance (:loc state) destination) (inc (:g parent)))))

(defn- smaller-f-in-list [state list]
  (let [matching (some #(if (= (:loc state) (:loc %)) %) list)]
    (if (nil? matching)
      false
      (> (:f state) (:f matching)))))

(defn- get-add-to-open-astar [open closed parent destination]
  (->> (get-possible parent [])
       (map #(set-vars % parent destination))
       (remove #(smaller-f-in-list % open))
       (remove #(smaller-f-in-list % closed))))

(defn- get-distance-astar
  ([input destination] (get-distance-astar [{:f 0 :g 0 :loc {:x 0 :y 0} :program (ic/string-to-program input)}] [] destination))
  ([open closed destination]
   (let [q (get-smallest open)]
     (if (= (:loc q) destination)
       (:g q)
       (recur (apply conj (remove #{q} open) (get-add-to-open-astar open closed q destination))
              (conj closed q)
              destination))
     )))

(defn puzzle1 [input] (get-distance-astar input (:loc (depth-first-find-location input))))

(defn- create-new-frontier [frontier visited]
  (->> frontier
       (map #(get-possible % visited))
       (flatten)))

(defn- spread-oxygen [frontier visited time]
  (if (empty? frontier)
    (dec time)
    (recur (create-new-frontier frontier visited)
           (apply conj visited (map :loc frontier))
           (inc time))))

(defn puzzle2 [input]
  (spread-oxygen [(depth-first-find-location input)] [] 0))
