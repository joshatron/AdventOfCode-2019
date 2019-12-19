(ns aoc-2019.day18
  (:require [clojure.string :as str]))

(defn- read-dungeon [input] (mapv #(mapv char %) (str/split input #"\n")))

(defn- find-start
  ([dungeon] (find-start dungeon 0))
  ([dungeon y]
   (if (some #{\@} (first dungeon))
     {:x (.indexOf (first dungeon) \@) :y y}
     (recur (rest dungeon) (inc y)))))

(defn- find-key
  ([dungeon key] (find-key dungeon key 0))
  ([dungeon key y]
   (if (some #{key} (first dungeon))
     {:x (.indexOf (first dungeon) key) :y y}
     (recur (rest dungeon) key (inc y)))))

(defn- get-at-location [dungeon loc] (get (get dungeon (:y loc)) (:x loc)))

(defn- is-key-at-loc [dungeon loc] (Character/isLowerCase (get-at-location dungeon loc)))

(defn- is-door-at-loc [dungeon loc] (Character/isUpperCase (get-at-location dungeon loc)))

(defn- legal-loc [dungeon loc] (not= (get-at-location dungeon loc) \#))

(defn- move-loc [loc dir]
  (case dir
    1 (assoc loc :y (inc (:y loc)))
    2 (assoc loc :y (dec (:y loc)))
    3 (assoc loc :x (dec (:x loc)))
    4 (assoc loc :x (inc (:x loc)))))

(defn- try-move [dungeon loc dir visited]
  (let [moved (move-loc loc dir)]
    (cond
      (some #{(dissoc moved :passed)} visited) nil
      (legal-loc dungeon moved) moved
      :else nil)))

(defn- get-possible [dungeon loc visited] (remove nil? (map #(try-move dungeon loc % visited) [1 2 3 4])))

(defn- is-unused-key-at-loc [dungeon gone loc]
  (and (not-any? #{(get-at-location dungeon loc)} gone)
       (is-key-at-loc dungeon loc)))

(defn- is-unused-door-at-loc [dungeon gone loc]
  (and (not-any? #{(get-at-location dungeon loc)} gone)
       (is-door-at-loc dungeon loc)))

(defn- assoc-key-or-door-to-loc [dungeon gone loc]
  (if (or (is-unused-key-at-loc dungeon gone loc)
          (is-unused-door-at-loc dungeon gone loc))
    (assoc loc :passed (conj (:passed loc) (get-at-location dungeon loc)))
    loc))

(defn- create-new-frontier [dungeon frontier visited gone]
  (->> frontier
       (map #(assoc-key-or-door-to-loc dungeon gone %))
       (map #(get-possible dungeon % visited))
       (flatten)
       (set)))

(defn- get-keys [dungeon locs gone]
  (->> locs
       (filter #(is-key-at-loc dungeon %))
       (remove #(some #{(get-at-location dungeon %)} gone))
       (map #(assoc % :key (get-at-location dungeon %)))
       ))

(defn- find-all-keys
  ([dungeon gone] (find-all-keys dungeon [(assoc (find-start dungeon) :passed [])] [] gone []))
  ([dungeon frontier visited gone keys]
  (if (empty? frontier)
    keys
    (recur dungeon
           (create-new-frontier dungeon frontier visited gone)
           (apply conj visited (map #(dissoc % :passed) frontier))
           gone
           (apply conj keys (get-keys dungeon frontier gone))))))

(defn- get-next-possible-keys [all-keys gone]
  (->> all-keys
       (remove #(some #{(:key %)} gone))
       (filter #(empty? (remove (set gone) (:passed %))))
       (mapv :key)))

(defn- get-key-options [all-keys keys gone]
  (let [next-possible (get-next-possible-keys all-keys gone)]
    (if (empty? next-possible)
      [keys]
      (->> next-possible
           (map #(get-key-options all-keys (conj keys %) (conj gone % (Character/toUpperCase %))))
           (reduce #(apply conj %1 %2))))))

(defn- get-next-keys [all-keys] (map :key (filter #(empty? (:passed %)) all-keys)))

(defn- get-blocked-by-key [all-keys key] (filter #(some #{(Character/toUpperCase key)} (:passed %)) all-keys))

(defn- get-blocking-behind-key [all-keys key] (filter #(and (some (fn [c] (Character/isUpperCase c)) (:passed %)) (some #{key} (:passed %))) all-keys))

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

(defn- get-add-to-open-astar [dungeon open closed parent destination]
  (->> (get-possible dungeon (:loc parent) [])
       (map (fn [l] {:loc l}))
       (map #(set-vars % parent destination))
       (remove #(smaller-f-in-list % open))
       (remove #(smaller-f-in-list % closed))))

(defn- get-distance-astar
  ([dungeon start destination] (get-distance-astar dungeon [{:f 0 :g 0 :loc start}] [] destination))
  ([dungeon open closed destination]
   (let [q (get-smallest open)]
     (if (= (:loc q) destination)
       (:g q)
       (recur dungeon
              (apply conj (remove #{q} open) (get-add-to-open-astar dungeon open closed q destination))
              (conj closed q)
              destination)))))

(defn- update-distance [dungeon current key]
  {:loc (find-key dungeon key)
   :dist (+ (:dist current) (get-distance-astar dungeon (:loc current) (find-key dungeon key)))})

(defn- get-distances [dungeon keys]
  (let [current {:loc (find-start dungeon) :dist 0}]
    (reduce #(update-distance dungeon %1 %2) current keys)))

(defn- pick-next-key [dungeon all-keys last-loc]
  (let [next-keys (get-next-keys all-keys)]
    (reduce #(cond
               (< (count (get-blocking-behind-key all-keys %1)) (count (get-blocking-behind-key all-keys %2))) %1
               (> (count (get-blocking-behind-key all-keys %1)) (count (get-blocking-behind-key all-keys %2))) %2
               (> (count (get-blocked-by-key all-keys %1)) (count (get-blocked-by-key all-keys %2))) %1
               (< (count (get-blocked-by-key all-keys %1)) (count (get-blocked-by-key all-keys %2))) %2
               :else %1)
            next-keys)))

(defn- get-key-pickup-order [dungeon keys found]
  (let [keys-left (find-all-keys dungeon found)]
    (if (empty? keys-left)
      keys
      (let [next-key (pick-next-key dungeon keys-left (find-key dungeon (last keys)))]
        (recur dungeon (conj keys next-key) (conj found next-key (Character/toUpperCase next-key)))))))

(defn puzzle1 [input] (count (get-key-options (find-all-keys (read-dungeon input) []) [] [])))

(defn puzzle2 [input] )
