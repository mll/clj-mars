(ns clj-mars.setup
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.spec.alpha :as spec]))

; op-sequence validates a warrior without an END instruction. 

(spec/def :address/mode #{:direct :indirect :pre-decrement-indirect :immediate})
(spec/def :address/target int?)
(spec/def :op/address (spec/keys :req-un [:address/mode :address/target]))

(spec/def :op/a :op/address)
(spec/def :op/b :op/address)
(spec/def :op/op #{:DAT :MOV :ADD :SUB :JMP :JMZ :JMN :CMP :SLT :DJN :SPL})

(spec/def ::op (spec/keys :req-un [:op/op :op/a :op/b]))

(spec/def ::op-sequence (spec/+ ::op))

(spec/def :warrior/active-process int?)
(spec/def :warrior/processes (spec/+ int?))
(spec/def :warrior/fields-owned set?)

(spec/def ::warrior (spec/keys :req-un [:warrior/active-process :warrior/processes :warrior/fields-owned]))

(spec/def :game/core ::op-sequence)
(spec/def :game/core-size int?)
(spec/def :game/wolf ::warrior)
(spec/def :game/cock ::warrior)
(spec/def :game/game-over boolean?)
(spec/def :game/loser keyword?)

(spec/def ::game (spec/keys :req-un 
                            [:game/core :game/core-size :game/wolf :game/cock]
                            :opt-un [:game/game-over :game/loser])) 

(defrecord Addressing 
    [target mode] 
  Object
  (toString [_] (str (case mode :immediate "#" :indirect "@" :pre-decrement-indirect "<" :direct "") target)))

(defrecord Op [op a b]
  Object
  (toString [_] (str (name op) " " a ", " b)))

(defn map->Op [op-map]
  (->Op (:op op-map)
        (map->Addressing (:a op-map))
        (map->Addressing (:b op-map))))

(defn- dat [] (map->Op {:op :DAT 
                        :a {:target 0 :mode :immediate}
                        :b {:target 0 :mode :immediate}}))

(defn- overlaps? [s1 e1 s2 e2 size]
  (let [set1 (if (< s1 e1) 
               (set (map #(+ s1 %) (range (- e1 s1))))
               (set/union (set (range e1)) (set (map #(+ s1 %) (range (- size s1))))))
        set2 (if (< s2 e2) 
               (set (map #(+ s2 %) (range (- e2 s2))))
               (set/union (set (range e2)) (set (map #(+ s2 %) (range (- size s2))))))]
    (not (empty? (set/intersection set1 set2)))))

(defn- install-warrior [core warrior start-pos]
  (let [warrior (vec (map map->Op warrior))
        l (count warrior)
        s (count core)]
    (loop [i 0 c core]
      (if (< i l)
        (recur (inc i) (assoc c (mod (+ i start-pos) s) (get warrior i)))
        c))))

(defn start-game [core-size wolf cock]
  (let [{wolf :warrior wolf-offset :start-offset} wolf
        {cock :warrior cock-offset :start-offset} cock
        _ (assert (spec/valid? ::op-sequence wolf) (spec/explain ::op-sequence wolf))
        _ (assert (spec/valid? ::op-sequence cock) (spec/explain ::op-sequence cock))
        core (vec (repeat core-size (dat)))
        wolf-start (rand-int core-size)
        wolf-end (mod (+ wolf-start (count wolf)) core-size)
        [cock-start cock-end] (loop [s (rand-int core-size)]
                                (let [e (mod (+ s (count cock)) core-size)]
                                  (if (overlaps? wolf-start wolf-end s e core-size)
                                    (recur (rand-int core-size))
                                    [s e])))
        game {:core (-> core 
               (install-warrior wolf wolf-start)
               (install-warrior cock cock-start))
              :core-size core-size
              :wolf {:active-process 0 
                     :processes [(+ wolf-start wolf-offset)]
                     :fields-owned (set (map #(mod (+ wolf-start %) core-size) 
                                             (range (count wolf))))}
              :cock {:active-process 0 
                     :processes [(+ cock-start cock-offset)] 
                     :fields-owned (set (map #(mod (+ cock-start %) core-size) 
                                             (range (count cock))))}}]
    ;; Validation is slow, but we leave it on for now to make sure all is ok.
    (assert (spec/valid? ::game game) (spec/explain ::game game))
    game))


