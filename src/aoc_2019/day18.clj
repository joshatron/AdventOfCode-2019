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

(defn- list-all-keys [dungeon]
  (->> dungeon
       (flatten)
       (filter #(Character/isLowerCase %))))

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

(defn- get-keys [dungeon locs gone steps]
  (->> locs
       (filter #(is-key-at-loc dungeon %))
       (remove #(some #{(get-at-location dungeon %)} gone))
       (map (fn [l] [(get-at-location dungeon l) (assoc l :key (get-at-location dungeon l) :steps steps)]))
       (into (sorted-map))))

(defn- find-all-keys
  ([dungeon start] (find-all-keys dungeon [(assoc (find-key dungeon start) :passed [])] [] [start] {} 0))
  ([dungeon frontier visited gone keys steps]
  (if (empty? frontier)
    keys
    (recur dungeon
           (create-new-frontier dungeon frontier visited gone)
           (apply conj visited (map #(dissoc % :passed) frontier))
           gone
           (conj keys (get-keys dungeon frontier gone steps))
           (inc steps)))))

(defn- find-all-key-combos [dungeon]
  (as-> (list-all-keys dungeon) k
       (conj k \@)
       (map (fn [k] [k (find-all-keys dungeon k)]) k)
       (into (sorted-map) k)))

(defn- get-next-possible-keys [all-keys gone]
  (->> all-keys
       (remove #(some #{(first %)} gone))
       (filter #(empty? (remove (set gone) (:passed (second %)))))
       (mapv first)))

(defn- find-shortest-path
  ([key-combos] (find-shortest-path key-combos \@ #{} 0))
  ([key-combos current-key gone dist-travelled]
   (let [next-keys (get-next-possible-keys (get key-combos current-key) gone)]
     (if (empty? next-keys)
       dist-travelled
       (->> next-keys
            (map #(find-shortest-path key-combos
                                      %
                                      (conj gone % (Character/toUpperCase %))
                                      (+ dist-travelled (:steps (get (get key-combos current-key) %)))))
            (apply min))))))

(defn- get-smallest [open] (reduce #(if (< (:f %1) (:f %2)) %1 %2) open))

(defn- set-vars [key-combos parent new-one]
  (let [g (+ (:g parent) (:steps (get (get key-combos (:current parent)) new-one)))]
  {:keys (conj (:keys parent) new-one)
   :gone (conj (:gone parent) new-one (Character/toUpperCase new-one))
   :g g
   :f (+ g (- (count key-combos) (count (:keys parent))))
   :current new-one}))

(defn- smaller-f-in-list [state list]
  (let [matching (some #(if (and (= (:keys state) (:keys %)) (= (:current state) (:current %))) %) list)]
    (if (nil? matching)
      false
      (> (:f state) (:f matching)))))

(defn- add-to-open [open closed q key-combos]
  (->> (get-next-possible-keys (get key-combos (:current q)) (:gone q))
       (map #(set-vars key-combos q %))
       (remove #(smaller-f-in-list % open))
       (remove #(smaller-f-in-list % closed))))

(defn- find-shortest-path-astar
  ([key-combos] (find-shortest-path-astar key-combos [{:f 0 :g 0 :keys #{} :gone #{} :current \@}] []))
  ([key-combos gone] (find-shortest-path-astar key-combos [{:f 0 :g 0 :keys #{} :gone gone :current \@}] []))
  ([key-combos open closed]
   (let [q (get-smallest open)]
     (if (= (count (:keys q)) (count (get key-combos \@)))
       (:g q)
       (recur key-combos
              (apply conj (remove #{q} open) (add-to-open open closed q key-combos))
              (conj closed q))))))

(defn puzzle1 [input] (find-shortest-path-astar (find-all-key-combos (read-dungeon input))))

(defn- separate-dungeon [dungeon center]
  (let [above (get dungeon (dec (:y center)))
        at (get dungeon (:y center))
        below (get dungeon (inc (:y center)))]
    (assoc dungeon (dec (:y center)) (assoc above (:x center) \#)
                   (:y center) (assoc at (dec (:x center)) \# (:x center) \# (inc (:x center)) \#)
                   (inc (:y center)) (assoc below (:x center) \#))))

(defn- get-new-dungeons [dungeon]
  (let [center (find-key dungeon \@)
        new-dungeon (separate-dungeon dungeon center)]
    (->> [[-1 -1] [-1 1] [1 -1] [1 1]]
         (map (fn [n] {:x (+ (first n) (:x center))
                       :y (+ (second n) (:y center))}))
         (map #(assoc new-dungeon (:y %) (assoc (get new-dungeon (:y %)) (:x %) \@))))))

(defn- get-all-split-key-combos [dungeon]
  (->> dungeon
       (get-new-dungeons)
       (map find-all-key-combos)))

(defn- get-in-other-quadrants [key-combos]
  (let [all (keys key-combos)
        in-quadrant (keys (get key-combos \@))
        difference (remove (set in-quadrant) all)]
    (set (concat difference (map #(Character/toUpperCase %) difference)))))

(defn- get-all-robots-dist [dungeon]
  (->> dungeon
       (get-all-split-key-combos)
       (map #(find-shortest-path-astar % (get-in-other-quadrants %)))
       (reduce +)))

(defn puzzle2 [input] (get-all-robots-dist (read-dungeon input)))
