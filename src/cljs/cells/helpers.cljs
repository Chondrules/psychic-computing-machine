(ns cells.helpers
  (:require 
     [reagent.core :as reagent :refer [atom]] ))

; make a cell of the form {:x x :y y :alive false}
(defn makeCell
  [x y living]
    (hash-map :x x :y y :alive living))

(defn getCellIndex
  [x y dim]
  (int (+ (* y dim) x)))

(defn getCell
  "Lots of stuff calls this to get a cell from the list.  Can it go faster"
  [x y board dim]
  (let [index (getCellIndex x y dim)]
     (nth board index)))

(defn getCells
  [vecXY board dim]
  (for [[x y] vecXY]
    (getCell x y board dim)))

(defn justTheLiving
  [cells]
  (filter :alive cells))

(defn infect
  "Make the cell alive"
  [c]
  (assoc c :alive true))

(defn findIndicies 
  [xyVec dim]
   (map #(getCellIndex (first %) (second %) dim) xyVec))

(defn showGrid
  "For debug, show some data in a grid"
  [data dim]
  (doseq [line (partition dim data)]
    (println line)))
