(ns aoc-2019.day13
  (:require [aoc-2019.intcomp :as ic]))

(defn- update-image
  ([outputs] (update-image outputs {}))
  ([outputs tiles]
  (if (empty? outputs)
    tiles
    (update-image (nthrest outputs 3)
                  (case (nth outputs 2)
                    0 (assoc tiles :blocks (filter #(or (not= (first outputs) (:x %)) (not= (second outputs) (:y %))) (:blocks tiles)))
                    1 (assoc tiles :walls (conj (:walls tiles) {:x (first outputs) :y (second outputs)}))
                    2 (assoc tiles :blocks (conj (:blocks tiles) {:x (first outputs) :y (second outputs)}))
                    3 (assoc tiles :paddle {:x (first outputs) :y (second outputs)})
                    4 (assoc tiles :ball {:x (first outputs) :y (second outputs)})
                    (assoc tiles :score (nth outputs 2)))))))

(defn puzzle1 [input]
  (-> input
        (ic/string-to-program)
        (ic/process-program-till-halt-or-input [])
        (:output)
        (update-image)
        (:blocks)
        (count)))

(defn- get-point-char [points x y]
  (cond
    (some #{{:x x :y y}} (:walls points)) "##"
    (some #{{:x x :y y}} (:blocks points)) "[]"
    (= (:paddle points) {:x x :y y}) "--"
    (= (:ball points) {:x x :y y}) "()"
    :else "  "))

(defn- draw-point [points x y max-x]
  (if (= x max-x)
    (println (get-point-char points x y))
    (print (get-point-char points x y))))

(defn- draw-output [points]
  (let [max-x (apply max (map :x (:walls points)))
        max-y (apply max (map :y (:walls points)))]
    (do
      (doseq [y (take (inc max-y) (iterate inc 0))
              x (take (inc max-x) (iterate inc 0))]
        (draw-point points x y max-x))
      (println "Score:" (:score points))
      points)))

(defn- setup-program [input]
  (-> input
      (ic/string-to-program)
      (assoc 0 2)
      (ic/process-program-till-halt-or-input [])))

(defn- setup-game [input]
  (let [initial-program (setup-program input)]
    {:program initial-program
     :points (update-image (:output initial-program))}))

(defn- run-game-step [game-state move]
  (let [new-program-state (ic/process-program-till-halt-or-input (:program game-state) [move])]
    {:program new-program-state
     :points (update-image (:output new-program-state) (:points game-state))}))

(defn- get-paddle-direction [game-state]
  (let [paddle-loc (:x (:paddle (:points game-state)))
        ball-loc (:x (:ball (:points game-state)))]
    (cond
      (< paddle-loc ball-loc) 1
      (> paddle-loc ball-loc) -1
      :else 0)))

(defn- run-program-till-halt [game-state display]
  (if (:done (:program game-state))
    game-state
    (if display
    (do
      (draw-output (:points game-state))
      (Thread/sleep 75)
      (recur (run-game-step game-state (get-paddle-direction game-state)) display))
    (recur (run-game-step game-state (get-paddle-direction game-state)) display))))

(defn puzzle2 [input]
  (:score (:points (run-program-till-halt (setup-game input) false))))
