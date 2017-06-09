(ns cells.game 
  (:require [cells.helpers :as helpers]
            [reagent.core :as reagent :refer [atom]]
  ))

(enable-console-print!)

; consider letting the user add cells.
(defonce app-state (reagent/atom {
  :dim 25 ; width and height in rows and cols, always square
  :boardWidth 500
  :boardHeight 500
  :fps 3
  :seeds (vector [2 3] [2 2] [5 2] [5 3] [4 3] [4 4] 
                 [6 6] [8 9] [8 8] [9 8] [9 10] [10 5]
                 [12 11] [11 11] [11 12] [11 13] [12 12] [13 13])
  :lastBoard []
  :started false
  }))

(defn validCoord 
  [xy]
  (let [x (first xy)
        y (second xy)]
  (and (>= x 0) (< x (:dim @app-state))
       (>= y 0) (< y (:dim @app-state)))))

; probably we should take some seeds as an option and speed up aggregation
; should we pull a let for the :dim @app-state
(defn buildBoard []
  "Range over n in both directions and generate a starting board"
  (into []
    (let [s (:dim @app-state)]
      (for [y (range s)
            x (range s)]
      (helpers/makeCell x y false)))))

(defn update-vals [map vals f]
" update the board by reducing over the updated cells"
  (reduce #(update-in % [%2] f %2) map vals))

(defn seedBoard
  "Inject cells into an existing board to seed initial values"
  [board infectThese]
  (let [b (into [] board)]
    (update-vals b (helpers/findIndicies (:seeds @app-state) (:dim @app-state)) helpers/infect)))

(defn- genNeighbors 
  "Return neighbor cells of given cell within the bounds of the board using dim from state to clip."
  [cell board]
  (helpers/getCells 
    (into []
      (filter validCoord ; filter out non valid coordinates after the list comp
        (for [dx [-1 0 1] 
          dy (if (zero? dx) [-1 1] [-1 0 1])
          :let [coord [(+ dx (:x cell)) 
                       (+ dy (:y cell))]]] 
        coord)
       )) 
       board (:dim @app-state)))

(defn- countNeighbors
  "Count the number of neighbors that are alive in the sequence"
  [neighborCells] 
  (reduce + (map :alive neighborCells)))

(defn makeNeighborMap
  [board]
  "Iterate over the board and create a count of alive neighbors for each cell.  Return vec of neighbors
  the same size as the board."
   (into []
     (map #(countNeighbors (genNeighbors % board)) board)))

(defn mutateCell
  [x y isAlive adjacentAlive]
  "Clean up all the do's here"
  (do 
    (if isAlive
      (do 
        (cond
          (< adjacentAlive 2) (helpers/makeCell x y false)
          (or (= adjacentAlive 3) (= adjacentAlive 2)) (helpers/makeCell x y true)
          (>= adjacentAlive 4) (helpers/makeCell x y false)
          :else (helpers/makeCell x y false))) ; just another empty cell
      (do 
        (if (= adjacentAlive 3) 
          (helpers/makeCell x y true)
          (helpers/makeCell x y false)))
        )))

(defn genNextBoard
  [lastBoard]
  (let [neighborMap (makeNeighborMap lastBoard)]
    (for [y (range (:dim @app-state))
          x (range (:dim @app-state))
          :let [numNeighbors (nth neighborMap (helpers/getCellIndex x y (:dim @app-state)))
                isAlive (:alive (helpers/getCell x y lastBoard (:dim @app-state)))]]
          (mutateCell x y isAlive numNeighbors))))
