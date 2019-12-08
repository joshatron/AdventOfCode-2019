(ns aoc-2019.day8)

(defn- getLayer
  [input width height output]
  (if (not= height 0)
    (recur (subs input width) width (dec height) (conj output (mapv #(Character/digit % 10) (seq (take width input)))))
    output))

(defn- splitIntoLayers
  [input layers width height]
  (if (empty? input)
    layers
    (recur (subs input (* width height)) (conj layers (getLayer input width height [])) width height)))

(defn- countDigitInLayer
  [layer digit total]
  (cond
    (empty? layer) total
    (empty? (first layer)) (recur (subvec layer 1) digit total)
    :else (recur (assoc layer 0 (subvec (first layer) 1))
                 digit
                 (if (= digit (first (first layer)))
                   (inc total)
                   total))))

(defn- getLayerWithLeast0s
  [layers best bestNum]
  (cond
    (empty? layers) best
    (< (countDigitInLayer (first layers) 0 0) bestNum) (recur (rest layers) (first layers) (countDigitInLayer (first layers) 0 0))
    :else (recur (rest layers) best bestNum)))

(defn puzzle1
  [input]
  (let [layer (getLayerWithLeast0s (splitIntoLayers input [] 25 6) [] 9999999)]
    (* (countDigitInLayer layer 1 0) (countDigitInLayer layer 2 0))))

(defn- getPixel
  [layers x y]
  (let [pixel (get (get (first layers) y) x)]
    (if (= pixel 2)
      (recur (rest layers) x y)
      (if (= pixel 1)
        "#"
        " "))))

(defn- createImageStrFromLayersRecur
  [layers image width height currentX currentY]
  (cond
    (and (= width currentX) (= height currentY)) image
    (= width currentX) (recur layers (str image "\n") width height 0 (inc currentY))
    :else (recur layers (str image (getPixel layers currentX currentY)) width height (inc currentX) currentY)))

(defn- createImageStrFromLayers
  [layers width height]
  (createImageStrFromLayersRecur layers "" width height 0 0))

(defn puzzle2
  [input]
  (let [layers (splitIntoLayers input [] 25 6)]
    (createImageStrFromLayers layers 25 6)))
