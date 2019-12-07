(ns aoc-2019.day6
  (:require [clojure.string :as str]))

(defn- createOrbitMapRecur
  [orbits root]
  {root
   (into (sorted-map) (map #(createOrbitMapRecur orbits %) (map #(get % 1) (filter #(= (get % 0) root) orbits))))
   })

(defn- createOrbitMap
  [orbits]
  (createOrbitMapRecur orbits "COM"))

(defn- countOrbitMapRecur
  [orbitMap depth]
  (if (empty? orbitMap)
    0
    (+ (* depth (count orbitMap)) (apply + (map #(countOrbitMapRecur % (inc depth)) (map #(get % 1) (seq orbitMap)))))))

(defn- countOrbitMap
  [orbitMap]
  (countOrbitMapRecur orbitMap 0))

(defn puzzle1
  [input]
  (countOrbitMap (createOrbitMap (map #(str/split % #"\)") (str/split input #"\n")))))

(defn- mapToNode
  [orbitMap currentPath toFind]
  (cond
    (empty? orbitMap) nil
    (contains? orbitMap toFind) currentPath
    :else (first (remove empty? (map #(mapToNode (get % 1) (conj currentPath (get % 0)) toFind) (seq orbitMap))))
    ))

(defn- findDistance
  [firstPath secondPath]
  (if (= (first firstPath) (first secondPath))
    (findDistance (rest firstPath) (rest secondPath))
    (+ (count firstPath) (count secondPath))))

(defn puzzle2
  [input]
  (let [orbitMap (createOrbitMap (map #(str/split % #"\)") (str/split input #"\n")))
        youPath (mapToNode orbitMap [] "YOU")
        sanPath (mapToNode orbitMap [] "SAN")]
    (findDistance youPath sanPath)))
