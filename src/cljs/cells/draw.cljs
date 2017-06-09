(ns cells.draw
  (:require [cells.helpers :as helpers]
            [cells.game :as game]))

(def uniqkey (atom 0))
(defn gen-key []
  (let [res (swap! uniqkey inc)]
    res))

(defn justTheLiving
  [cells]
  (filter :alive cells))

(defn vLineCoords
  "generate the vector of [x y w h] values that make up the vertical lines of the board"
  [height divisions] 
    (into []
    (let [step (/ height divisions)]
      (for [x (range step height step)]
        [(int x) 0 1 height]))))

(defn hLineCoords
  "generate the vector of [x y w h] values that make up the horizontal lines of the board"
  [width divisions]
    (into []
    (let [step (/ width divisions)]
      (for [y (range step width step)]
        [0 (int y) width 1]))))

(defn makeSVGRects
    "Take a vector of vectors, the item in each vector is [x y width height] of a rectangle and 
    generate hiccup style svg for it."
    [vecXYWH]
    (into [] 
        (for [[x y w h] vecXYWH]
           ^{:key (gen-key)} [:rect 
                (hash-map :x x :y y :width w :height h)])))

(defn mapPixelToRect
  "return [x1 y1 w h] of pixel rectangle to draw."
  [x y boardSizePixels dim]
    (let [pixelSize (/ boardSizePixels dim)]
      [(* x pixelSize) (* y pixelSize) pixelSize pixelSize]))

(defn getLivingCellCoords
  "get a vec of living cells and convert each cell to a [x y w h] into []"
  [board dim]
    (into [] 
      (let [bbb (justTheLiving board)]
        (map #(mapPixelToRect (:x %) (:y %) (:boardWidth @game/app-state) dim) bbb))))
