
;; TODO:

;; v 1. EQU 
;; v 2. EQU i END - kontrola syntaxu (bo nie robimy tego w gramatyce. A moze powinnismy?) 
;; v 3. pozostale opsy
;; v 4. Biblioteka do UI (clojure2d)
;; 

(ns clj-mars.core
  (:require [corewars.load :as load-warrior]
            [corewars.setup :as setup]
            [corewars.execute :as execute]
            [corewars.draw :as draw]
;            [clojure2d.core :as c2d]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as s])
  (:gen-class))


(defn -main
  "For now just loading a warrior."
  [& args]
  (assert (= (count args) 2) "The names of two warrior .red files need to be passed as arguments")
  (let [_ (println "Loading first warrior...")
        wolf (load-warrior/load-warrior (first args))
        _ (println "Loading second warrior...")
        cock (load-warrior/load-warrior (second args))
        _ (println "Starting game...")
        _ (println (map #(str % "\n") (map setup/map->Op (:warrior cock))))
        game (atom (setup/start-game 8000 wolf cock))
        iterations-before-draw 100000
        speed (atom 10)
        [canvas window] (draw/show game)
        result (future (loop [i 0]
                         (if (:game-over @game)
                           @game
                           (if (>= i iterations-before-draw)
                             (assoc @game :game-over true)
                             (do
                               (Thread/sleep @speed) 
                               (swap! game #(execute/game-step %)) 
                               (recur (inc i)))))))
        final @result]
        
    (println "Game over! " (if (:loser final) (str " Winner: " (if (= (:loser final) :cock) (str (first args) " (left)") (str (second args) " (right)"))) "Draw!"))

;    (println (:cock final))
 ;   (println (:wolf final))
;    (c2d/close-window window)
    
    ))


