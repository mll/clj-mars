(ns clj-mars.core
  (:require [clj-mars.load :as load-warrior]
            [clj-mars.setup :as setup]
            [clj-mars.execute :as execute]
            [clj-mars.draw :as draw]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as s])
  (:gen-class))


(defn -main
  "For now just loading a warrior."
  [& args]
  (assert (= (count args) 2) "The names of two warrior .red files need to be passed as arguments")
  (let [parameters (read-string (slurp (clojure.java.io/resource "config.edn")))        
        _ (println "Loading first warrior...")
        wolf (load-warrior/load-warrior (first args) parameters)
        _ (println "Loading second warrior...")
        cock (load-warrior/load-warrior (second args) parameters)
        _ (println "Starting game...")
        _ (println (map #(str % "\n") (map setup/map->Op (:warrior cock))))
        game (atom (setup/start-game parameters wolf cock))
        iterations-before-draw (:MAXCYCLES parameters)
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


