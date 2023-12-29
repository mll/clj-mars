;; Loads warriors
(ns clj-mars.load
  (:require [instaparse.core :as insta]
            [instaparse.failure :as insta-failure]
            [clojure.string :refer [join upper-case]]
            [clojure.walk :refer [postwalk prewalk]]
            [clojure.set :refer [intersection union difference]]
            [clj-mars.setup :refer [map->Op]]))

(def parser (insta/parser (clojure.java.io/resource "icws88.bnf")))

(defmulti build-ast (fn [node _] (first node)))

(defn node-type? [node types]
  (and (sequential? node)
       (types (first node))))

(defn arg-of-type [node types]
  (first (filter #(node-type? % types) (rest node))))

;; ====================== support for labels, EQU and END =========================

(defn extract-label [terminated-instruction]
  (let [label? (get-in terminated-instruction [1 1])]
    ;label is always first
    (when (node-type? label? #{:label})
      (-> label?
       (arg-of-type #{:label-name})
       second))))


(defn equ? [terminated-instruction]
  (seq (arg-of-type (get terminated-instruction 1) #{:equ-instruction})))

(defn end? [terminated-instruction]
  (let [basic (arg-of-type (get terminated-instruction 1) #{:basic-instruction})]
    (seq (arg-of-type basic #{:end-instruction}))))

(defn equ-value [terminated-instruction extra]
  (if-let [equ (arg-of-type (get terminated-instruction 1) #{:equ-instruction})]
    (build-ast (last equ) extra)))

(defn end-value [terminated-instruction extra]
  (when (end? terminated-instruction)
    (let [basic (arg-of-type (get terminated-instruction 1) #{:basic-instruction})
          end (get basic 1)]
      (if-let [end-exp (arg-of-type end #{:expression})]
        (build-ast end-exp extra)))))

(defn extract-labels-map [programme] 
  (let [instructions (filter #(= :terminated-instruction (first %)) (rest programme))
;        _ (println instructions)
        labels (keep extract-label instructions)
        _ (doseq [[label cnt] (frequencies labels)]
            (when-not (= cnt 1) (throw (IllegalStateException. (str "Label '" label "' occurs more than once (" cnt ").")))))



        labels-map (into {} (apply merge (map-indexed (fn [idx inst]
                                                        (if-let [l (extract-label inst)]
                                                          {l idx})) 
                                                      (filter #(and (not (equ? %))
                                                                    (not (end? %))) 
                                                              instructions))))
        equ-map (into {} (apply merge (map #(hash-map (extract-label %) 
                                             (last (arg-of-type (get % 1) #{:equ-instruction}))) 
                                            (filter equ? instructions))))
        ]
    (merge equ-map labels-map)))

;; ============================== AST Processing =========================================

(defmethod build-ast :programme [node parameters] 
  (let [labels (extract-labels-map node)
        _ (assert (empty? (intersection (set (keys labels)) (set (keys parameters)))) 
                  (str "Labels in the programme cannot be named like game parameters: " (intersection (set (keys labels) (keys parameters)))))
        full-params (merge parameters labels)

        instructions (filter #(= :terminated-instruction (first %)) (rest node))
        non-equ-instructions (vec (filter (comp not equ?) instructions))
        end-idxs (filter identity 
                        (map-indexed (fn [idx inst] (when (end? inst) idx)) 
                                     non-equ-instructions))
        
        _ (when-not (<= (count end-idxs) 1) (throw (IllegalStateException. "The programme can have only one end statement.")))

        end-idx (first end-idxs)
        end-value (if end-idx (or (end-value (nth non-equ-instructions 
                                                 end-idx)
                                             (assoc full-params :CURLINE end-idx)) (- end-idx)) 0)
        
        final-instructions (if end-idx (take end-idx non-equ-instructions) 
                               non-equ-instructions)]
    {:warrior (map map->Op (map-indexed #(build-ast %2 (assoc full-params :CURLINE %1)) 
                                        final-instructions))
     :start-offset (if end-idx (+ end-idx end-value) 0)}))

(defmethod build-ast :terminated-instruction [node extra] (build-ast (second node) extra))
(defmethod build-ast :instruction [node extra] (build-ast (last node) extra))
(defmethod build-ast :label [node _] (get-in node [1 1]))
(defmethod build-ast :integer [node _] (Integer/parseInt (second node)))
(defmethod build-ast :address-op [node _] (get-in node [1 0]))
(defmethod build-ast :address [node extra] 
  {:target (build-ast (last node) extra)
   :mode (if (= 3 (count node))
           (build-ast (second node) extra)
           :direct)})

(defmethod build-ast :opcode [node _] (keyword (upper-case (second node))))

(defmethod build-ast :primary-instruction [node extra] 
  (let [args (map #(build-ast % extra) (rest node))]
    (if (= 2 (count args))
      {:op (first args)
       :a (last args)
       :b {:target 0 :mode :direct}}
      {:op (first args)
       :a (nth args 1)
       :b (nth args 2)})))


(defmethod build-ast :basic-instruction [node extra] (build-ast (last node) extra))

(defmethod build-ast :p2-op [node _] (case (second node) "+" + "-" -))
(defmethod build-ast :p1-op [node _] (case (second node) "*" * "/" (comp int /) "%" mod))

(defmethod build-ast :expression [node extra]
  (let [args (rest node)
        term1 (build-ast (first args) extra)]
    (if (and (node-type? (last args) #{:term})
             (> (count args) 1))
      (let [term2 (build-ast (last args) extra)
            op (build-ast (arg-of-type node #{:p2-op}) extra)]
        (op term1 term2)) 
      term1)))

(defmethod build-ast :term [node extra]
  (let [args (rest node)
        factor1 (build-ast (first args) extra)]
    (if (and (node-type? (last args) #{:factor})
             (> (count args) 1)) 
      (let [factor2 (build-ast (last args) extra)
            op (build-ast (arg-of-type node #{:p1-op}) extra)]
        (op factor1 factor2)) 
      factor1)))

(defmethod build-ast :factor [node extra]
  (let [args (rest node)]
    (cond (= (count args) 1) (build-ast (first args) extra)
          (arg-of-type node #{:expression}) (build-ast (arg-of-type node #{:expression}) extra)
          (arg-of-type node #{:label-name}) (if (= (build-ast (first args) extra) -) 
               (* -1 (build-ast (last args) extra))
               (build-ast (last args) extra))
          :default args)))

(defmethod build-ast :label-name [node extra] 
  (let [n (second node)
        label (get-in extra [n])
        current-offset (get-in extra [:CURLINE])
        equ? (sequential? label)]
    (when-not label (throw (IllegalStateException. (str "Label not found: " n))))
    (when-not current-offset (throw (IllegalStateException. "Logic error - no current offset")))
    (if equ? (build-ast label extra) (- label current-offset))))

(defmethod build-ast :default [_ _] nil)

(defn load-warrior 
  "Loads a warrior from a file. Produces a {:warrior ... :start-offset ...} map.
   The warrior is fully normalised, with all equ and end instructions removed." 
  [path parameters]
  (let [parsed (->> path
                  (slurp)
                  (insta/parse parser)
                  ; We reduce tags that were introduced in the grammar for the sole purpose of syntax checking
                  (postwalk #(case %
                                 :restricted-instruction :primary-instruction
                                 :restricted-opcode :opcode
                                 :restricted-address :address
                                 :restricted-address-op :address-op
                                 %))
                  (prewalk #(if (node-type? % #{:space :comma :empty-line :newline :comment "(" ")" ":"}) nil %))
                  (postwalk #(if (coll? %) (into (empty %) (remove nil? %)) %)))]    
    (when-not (vector? parsed) (throw (IllegalStateException. (with-out-str (insta-failure/pprint-failure parsed)))))
   ; (println parsed)
    (build-ast parsed parameters)
    ))
