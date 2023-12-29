(ns clj-mars.draw
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]))

;; be sure everything is fast as possible
;(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)
;(m/use-primitive-operators)

;; define canvas
;(def my-canvas (c2d/canvas 1200 1200))

;; create window
;(def window (c2d/show-window my-canvas "Hello World!"))
;; draw rectangle with line wrapping with threading canvas context
;; (c2d/with-canvas-> my-canvas ;; prepare drawing context in canvas
;;   (c2d/set-background 10 5 5) ;; clear background
;;   (c2d/set-color 210 210 200) ;; set color
;;   (c2d/rect 100 100 1 1) ;; draw rectangle
;;   (c2d/set-color 50 50 60) ;; set another color
;;   ;(c2d/set-stroke 2.0) ;; set line width
;;   (c2d/rect 50 300 1 1) ;; draw line
;;   (c2d/set-font-attributes 30.0) ;; set font size
;;   (c2d/set-color :maroon) ;; set current color
;;   (c2d/text "Hello World!" 110 130)) ;; draw line

;; (c2d/with-canvas-> my-canvas ;; prepare drawing context in canvas
;;   (c2d/set-color :maroon) ;; set current color
;;   (c2d/rect 55 305 5 5)

;;   ) ;; draw line

(def pi Math/PI)

(defn sierpinski [c left-bottom-x left-bottom-y side-length depth max-depth]
  (if (> depth max-depth) nil
  (let [angle (m/fast-identity (/ ^double pi 3))
        dx (m/fast* (m/cos angle) side-length)
        dy (m/fast* (m/sin angle) side-length)
        top-x (m/fast+ left-bottom-x dx)
        top-y (m/fast- left-bottom-y dy)
        right-x (m/fast+ top-x dx) 
        right-y (m/fast+ top-y dy)
        half-side (/ side-length 2.0)]
  (c2d/with-canvas-> c
    (c2d/set-color 0 0 0)
    (c2d/set-stroke 1.0 :round)
    (c2d/line left-bottom-x left-bottom-y top-x top-y)
    (c2d/line top-x top-y right-x right-y)
    (c2d/line left-bottom-x left-bottom-y right-x right-y))
  (sierpinski c left-bottom-x left-bottom-y half-side (inc depth) max-depth)
  (sierpinski c (+ left-bottom-x half-side) left-bottom-y half-side (inc depth) max-depth)
  (sierpinski c (+ left-bottom-x (/ half-side 2.0)) (- left-bottom-y (/ dy 2.0)) half-side (inc depth) max-depth))))

;(sierpinski my-canvas 150.0 945.0 1000.0 0 2)


(defn draw-game [canvas game]
  (let [w (- (c2d/width canvas) 50)
        h (- (c2d/height canvas) 50)
        core-dim (int (m/sqrt (:core-size game)))
        rect-size (int (/ h core-dim))]
    (c2d/with-canvas [c canvas]
      (c2d/set-color c :white)
      (c2d/rect c 0 0 (c2d/width c) (c2d/height c))
      (loop [i 0]
        (let [row (int (/ i core-dim))
              column (mod i core-dim)]
          (when-not (= i (:core-size game))
            (cond (contains? (get-in game [:wolf :fields-owned]) i)
                  (do (c2d/set-color c 200 200 200 255)
                      (c2d/rect c (inc (* column rect-size)) (inc (* row rect-size)) (- rect-size 2) (- rect-size 2)))
                  (contains? (get-in game [:cock :fields-owned]) i)
                   (do (c2d/set-color c 100 100 100 255)
                      (c2d/rect c (inc (* column rect-size)) (inc (* row rect-size)) (- rect-size 2) (- rect-size 2)))
                  :default nil)
            (cond (some #(= i %) (get-in game [:wolf :processes]))
                  (do (c2d/set-color c :blue)
                      (c2d/rect c (inc (* column rect-size)) (inc (* row rect-size)) (- rect-size 2) (- rect-size 2)))
                  (some #(= i %) (get-in game [:cock :processes]))
                   (do (c2d/set-color c :red)
                      (c2d/rect c (inc (* column rect-size)) (inc (* row rect-size)) (- rect-size 2) (- rect-size 2)))
                  :default nil)

            (c2d/set-color c 80 80 80 255)
            (c2d/rect c (* column rect-size) (* row rect-size) (dec rect-size) (dec rect-size) true)
            (recur (inc i)))
          )))))

(defn show [game-atom] 
  (let [c (c2d/canvas 1200 800)
        w (c2d/show-window c "Corewars" 30 (fn [& args] (draw-game c @game-atom)))]
    [c w]))



