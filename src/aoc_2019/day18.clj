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
  ([key-combos] (find-shortest-path key-combos \@ [] 0))
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

(defn puzzle1 [input] (find-shortest-path (find-all-key-combos (read-dungeon input))))

(defn puzzle2 [input] )
