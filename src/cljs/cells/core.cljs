(ns cells.core
    (:require [reagent.core :as r :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.dom :as dom]
              [goog.events :as events]
              [garden.core :as g]
              [cljs.core.async :refer [put! take! chan <! >! timeout close!]]
              [cells.game :as game]
              [cells.draw :as draw]
              [accountant.core :as accountant]))

;maybe consider garden for some css?

(defn newGame []
  "Seed a new game board with initial seeds in app-state"
  (let [board (game/buildBoard)]
    (game/seedBoard board (:seeds @game/app-state))))

(defn seedNewGame []
  (swap! game/app-state update-in [:lastBoard] newGame))

(defn setGamePlayingState [playing]
  "set game playing or not playing"
  (swap! game/app-state assoc :started playing))

(defn invertGamePlayingState []
  "inverted :started state in app-state"
  (setGamePlayingState (not (:started @game/app-state))))

(defn iterateGameBoard []
  "Generate the next game board state"
  (swap! game/app-state assoc :lastBoard (game/genNextBoard (:lastBoard @game/app-state))))

(defn startGameTimer []
  "Start the timeout loop that updates board state"
  (swap! game/app-state assoc :timer (js/setInterval iterateGameBoard 750)))

(defn stopGameTimer []
  "Stop the timeout loop that updates board state"
  (let [timer (:timer @game/app-state)]
    (js/clearTimeout timer)))

(defn resetGameState []
    (stopGameTimer)
    (setGamePlayingState false)
    (swap! game/app-state assoc :lastBoard nil))

(defn drawBoard 
  "generate a svg object with the board drawn using simple lines for horizontal and vertical"
  []
  [:svg {:x 0 :y 0 :width (:boardWidth @game/app-state) :height (:boardHeight @game/app-state)}
    (let [rects (draw/makeSVGRects (draw/vLineCoords (:boardWidth @game/app-state) (:dim @game/app-state)))]
      (map identity rects))
    (let [rects (draw/makeSVGRects (draw/hLineCoords (:boardHeight @game/app-state) (:dim @game/app-state)))]
      (map identity rects))
    (let [rects (draw/makeSVGRects (draw/getLivingCellCoords (:lastBoard @game/app-state) (:dim @game/app-state)))]
      (map identity rects))
  ])

(defn button-state?
  [started]
  "If we are paused, show play.  If we are playing show pause."
  (if (= false started) 
    "Start Game"
    "Pause Game"))

(defn playButton
  "add a button the user can press to start the game rolling."
  []
    [:div
     [:input
       {:type "button"
       :value (button-state? (:started @game/app-state))
       :on-click (fn []
            (invertGamePlayingState)
            (if (= true (:started @game/app-state))
                (startGameTimer)
                (stopGameTimer)))
        }]])

(defn resetGameButton
  "add a button the user can press to reset the game."
  []
    [:div
     [:input
       {:type "button"
       :value "Reset Game"
       :on-click (fn [e]
        (resetGameState)
        (seedNewGame))
       }]])

;handle keyCode 13 which is enter and reset the board state there.
(defn update-board-size
  "Modify the app-state dimension when the user types it in, use it to update board size"
  []
  [:div
    [:p "Enter a board size (15 to 50): "
     [:input
       {:type "number"
        :min 15
        :max 50 
        :step 1
        :defaultValue (:dim @game/app-state)
        :on-key-down (fn [e]
          (when (= (.-keyCode e) 13)
            (def theval (-> e .-target .-value))
            (resetGameState)
            (cond 
              (< theval 15) (js/alert "Must enter a number 15 or higher")
              (> theval 50) (js/alert "Must enter a number less than 50")
              :else (swap! game/app-state assoc :dim theval))
            (seedNewGame)))
       }]]])

; home-page is the root element fn.
(defn home-page []
    (fn []
      [:div {:class "nothing important"}
       [:h2 "Reagent Game of Life Cljs style"]
       [:table
        [:tbody
         [:tr [:td (update-board-size)] [:td (playButton)] [:td (resetGameButton)]]]]
       [:div (drawBoard)]
      ]))

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (seedNewGame)
  (mount-root))
