(ns aoc-2019.day20
  (:require [clojure.string :as str]))

(defn- read-maze [input] (mapv #(mapv char %) (str/split input #"\n")))

(defn- width [maze] (apply max (map count maze)))
(defn- height [maze] (count maze))

(defn- get-at-loc [maze loc]
  (try (get (get maze (:y loc)) (:x loc))
       (catch Exception e \#)))

;   0
; 3   1
;   2
(defn- move-loc [loc dir]
  (case dir
    0 (assoc loc :y (dec (:y loc)))
    1 (assoc loc :x (inc (:x loc)))
    2 (assoc loc :y (inc (:y loc)))
    3 (assoc loc :x (dec (:x loc)))))

(defn- find-letter [maze letter]
  (->> (take (* (width maze) (height maze)) (iterate inc 0))
       (map (fn [n] {:x (mod n (width maze)) :y (quot n (width maze))}))
       (filter #(= (get-at-loc maze %) letter))))

(defn- get-horizontal-endpoint [maze first-loc second-letter]
  (cond
    (not= (get-at-loc maze (move-loc first-loc 1)) second-letter) nil
    (= (get-at-loc maze (move-loc first-loc 3)) \.) (move-loc first-loc 3)
    (= (get-at-loc maze (move-loc (move-loc first-loc 1) 1)) \.) (move-loc (move-loc first-loc 1) 1)
    :else nil))

(defn- get-vertical-endpoint [maze first-loc second-letter]
  (cond
    (not= (get-at-loc maze (move-loc first-loc 2)) second-letter) nil
    (= (get-at-loc maze (move-loc first-loc 0)) \.) (move-loc first-loc 0)
    (= (get-at-loc maze (move-loc (move-loc first-loc 2) 2)) \.) (move-loc (move-loc first-loc 2) 2)
    :else nil))

(defn- get-endpoint [maze first-loc second-letter]
  (let [horizontal (get-horizontal-endpoint maze first-loc second-letter)
        vertical (get-vertical-endpoint maze first-loc second-letter)]
    (cond
      horizontal horizontal
      vertical vertical
      :else nil)))

(defn- find-portal [maze code]
  (->> (find-letter maze (first code))
       (map #(get-endpoint maze % (second code)))
       (remove nil?)))

(defn- legal-loc [maze loc] (not= (get-at-loc maze loc) \#))

(defn- get-portal-from-loc [maze original loc]
  (cond
    (< (:y loc) (:y original)) (find-portal maze [(get-at-loc maze (move-loc loc 0)) (get-at-loc maze loc)])
    (> (:y loc) (:y original)) (find-portal maze [(get-at-loc maze loc) (get-at-loc maze (move-loc loc 2))])
    (< (:x loc) (:x original)) (find-portal maze [(get-at-loc maze (move-loc loc 3)) (get-at-loc maze loc)])
    (> (:x loc) (:x original)) (find-portal maze [(get-at-loc maze loc) (get-at-loc maze (move-loc loc 1))])))

(defn- follow-portal [maze original loc]
  (->> (get-portal-from-loc maze original loc)
       (remove #{original})
       (first)))

(defn- check-for-portal [maze original loc]
  (if (= (get-at-loc maze loc) \.)
    loc
    (follow-portal maze original loc)))

(defn- visited? [visited loc]
  (if (empty? (get visited (:x loc)))
    false
    (not (nil? (get (get visited (:x loc)) (:y loc))))))

(defn- get-next [maze loc visited]
  (->> (iterate inc 0)
       (take 4)
       (map #(move-loc loc %))
       (filter #(legal-loc maze %))
       (map #(check-for-portal maze loc %))
       (remove nil?)
       (remove #(visited? visited %))))

(defn- expand-frontier [maze frontier visited]
  (set (flatten (pmap #(get-next maze % visited) frontier))))

(defn- update-visited-with-loc [visited loc]
  (assoc visited (:x loc) (assoc (get visited (:x loc)) (:y loc) 1)))

(defn- update-visited [visited new-locs]
  (if (empty? new-locs)
    visited
    (recur (update-visited-with-loc visited (first new-locs)) (rest new-locs))))

(defn- find-end-bfs
  ([maze] (find-end-bfs maze [(first (find-portal maze [\A \A]))] {} 0 (first (find-portal maze [\Z \Z]))))
  ([maze frontier visited steps end-location]
   (if (some #{end-location} frontier)
     steps
     (recur maze
            (expand-frontier maze frontier visited)
            (update-visited visited frontier)
            (inc steps)
            end-location))))

(defn puzzle1 [input] (find-end-bfs (read-maze input)))

(defn- find-recursive-portal [maze code]
  (->> (find-letter maze (first code))
       (map #(get-endpoint maze % (second code)))
       (remove nil?)))

(defn- get-recursive-portal-from-loc [maze original loc]
  (cond
    (< (:y loc) (:y original)) (find-recursive-portal maze [(get-at-loc maze (move-loc loc 0)) (get-at-loc maze loc)])
    (> (:y loc) (:y original)) (find-recursive-portal maze [(get-at-loc maze loc) (get-at-loc maze (move-loc loc 2))])
    (< (:x loc) (:x original)) (find-recursive-portal maze [(get-at-loc maze (move-loc loc 3)) (get-at-loc maze loc)])
    (> (:x loc) (:x original)) (find-recursive-portal maze [(get-at-loc maze loc) (get-at-loc maze (move-loc loc 1))])))

(defn- adjust-level [maze portal]
  (if (or (< (:x portal) 4) (< (:y portal) 4)
          (> (:x portal) (- (width maze) 5)) (> (:y portal) (- (height maze) 5)))
    (dec (:level portal))
    (inc (:level portal))))

(defn- follow-recursive-portal [maze original loc]
  (->> (get-recursive-portal-from-loc maze original loc)
       (map #(assoc % :level (adjust-level maze loc)))
       (remove #(< (:level %) 0))
       (remove #(and (= (:x original) (:x %)) (= (:y original) (:y %))))
       (first)))

(defn- check-for-recursive-portal [maze original loc]
  (if (= (get-at-loc maze loc) \.)
    loc
    (follow-recursive-portal maze original loc)))

(defn- get-recursive-visited [visited loc]
  (let [v (get (get visited (:x loc)) (:y loc))]
    (if (nil? v) [] v)))

(defn- recursive-visited? [visited loc]
  (some #{(:level loc)} (get-recursive-visited visited loc)))

(defn- get-recursive-next [maze loc visited]
  (->> (iterate inc 0)
       (take 4)
       (map #(move-loc loc %))
       (filter #(legal-loc maze %))
       (map #(check-for-recursive-portal maze loc %))
       (remove nil?)
       (remove #(recursive-visited? visited %))))

(defn- expand-recursive-frontier [maze frontier visited]
  (set (flatten (pmap #(get-recursive-next maze % visited) frontier))))

(defn- update-recursive-visited-with-loc [visited loc]
  (assoc visited (:x loc)
                 (assoc (get visited (:x loc)) (:y loc)
                                               (conj (get-recursive-visited visited loc) (:level loc)))))

(defn- update-recursive-visited [visited new-locs]
  (if (empty? new-locs)
    visited
    (recur (update-recursive-visited-with-loc visited (first new-locs)) (rest new-locs))))

(defn- get-start-loc [maze]
  (-> (find-portal maze [\A \A])
      (first)
      (assoc :level 0)))

(defn- get-end-loc [maze]
  (-> (find-portal maze [\Z \Z])
      (first)
      (assoc :level 0)))

(defn- find-recursive-end-bfs
  ([maze] (find-recursive-end-bfs maze [(get-start-loc maze)] {} 0 (get-end-loc maze)))
  ([maze frontier visited steps end-location]
   (if (some #{end-location} frontier)
     steps
     (recur maze
            (expand-recursive-frontier maze frontier visited)
            (update-recursive-visited visited frontier)
            (inc steps)
            end-location))))

(defn puzzle2 [input] (find-recursive-end-bfs (read-maze input)))
