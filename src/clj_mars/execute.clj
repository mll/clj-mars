(ns clj-mars.execute)

(defn jump 
  "Since every single command except for dat ends with a jump,
   we add process increment here"
  [game player active-process target]
  (let [processes (get-in game [player :processes])]
    (-> game
        (assoc-in [player :processes active-process] (mod target (:core-size game)))
        (assoc-in [player :active-process] (mod (inc active-process) (count processes))))))

(defn mark-field-ownership 
  "We need to keep track of who was the last player to write to a certain field (for drawing).
   This function marks a field as being last written to by a certain player."
  [game player field]
  (let [other-player (if (= player :wolf) :cock :wolf)]
    (-> game 
        (update-in [player :fields-owned] #(conj % field))
        (update-in [other-player :fields-owned] #(disj % field)))))

  ;; First   the   A   addressing   mode  is
  ;; examined. If  immediate,  we  are done.
  ;; Otherwise, the program counter is added
  ;; to the  value in  the A-field, yielding
  ;; an absolute address. If  direct, we are
  ;; done.  Otherwise,  the  B-field of this
  ;; direct memory location is read into the
  ;; A-field. If  predecrement is indicated,
  ;; the value just read  is decremented and
  ;; written  back  to  the  memory location
  ;; from  whence  it  came.   Finally,  the
  ;; program counter  is added  to the value
  ;; in the A-field.                        
                                       
  ;; At  this  point   the   A-field  either
  ;; contains  immediate  data  or  else  it
  ;; contains the absolute address  of the A
  ;; operand.  The  same  procedure  is  now
  ;; performed to normalise  the  B operand.
  ;; With  bother  operands  now normalised,
  ;; the operation code is  examined and the
  ;; specified operation performed.

(defn resolve-addressing [game address source player]
  (let [{:keys [target mode]} address
        direct-address (mod (+ source target) (:core-size game))
        ]
    {:resolved-target (case mode
                        :immediate source
                        :direct direct-address
                        :indirect (mod (+ direct-address (let [core (:core game)
                                                       pad (nth core (mod (+ source target) (:core-size game)))]
                                                   (-> pad :b :target))) (:core-size game))
                        :pre-decrement-indirect (mod (+ direct-address (let [core (:core game)
                                                                     pad (nth core (mod (+ source target) (:core-size game)))]
                                                                 (dec (-> pad :b :target)))) (:core-size game)))
     :immediate (when (= mode :immediate) target)
     :game (if (= :pre-decrement-indirect mode)
             (-> game 
                 (update-in [:core direct-address :b :target] dec)
                 (mark-field-ownership player direct-address))
             game)}))

(defn normalise-op [op-location game player]
  (let [op (get-in game [:core op-location])
        {a-target :resolved-target game :game a-immediate :immediate} (resolve-addressing game (:a op) op-location player)
        op (get-in game [:core op-location])
        {b-target :resolved-target game :game b-immediate :immediate} (resolve-addressing game (:b op) op-location player)]
    {:game game
     :a-target a-target
     :b-target b-target
     :a-immediate a-immediate
     :b-immediate b-immediate}))

(defmulti execute-op (fn [op op-location active-process player game] (:op op)))


(defmethod execute-op 

  ;;  ADD  A   B  :  If  the  A  operand  is
  ;;  immediate it  is added  to the B-field
  ;;  of the B operand.  If the A operand is
  ;;  not immediate both the  A-field and B-
  ;;  field  of  the  A  operand  are  added
  ;;  respectively  to  the  A-field  and B-
  ;;  field of the B operand. 

  :ADD 

  [op op-location active-process player game]
  (let [{:keys [a-target b-target game a-immediate]} (normalise-op op-location game player)

        new-game (if a-immediate                   
                   (update-in game [:core b-target :b :target] #(+ % a-immediate))
                   (let [a-a (get-in game [:core a-target :a :target])
                         a-b (get-in game [:core a-target :b :target])]
                     (-> game
                         (update-in [:core b-target :a :target] #(+ % a-a))
                         (update-in [:core b-target :b :target] #(+ % a-b)))))]
    (-> new-game
        (mark-field-ownership player b-target)
        (jump player active-process (inc op-location)))))

(defmethod execute-op 

  ;;  SUB  A   B  :  If  the  A  operand  is
  ;;  immediate it is subtracted from the B-
  ;;  field  of  the  B  operand.  If  the A
  ;;  operand is  not immediate  both the A-
  ;;  field and B-field of the A operand are
  ;;  subtracted  respectively  from  the A-
  ;;  field and B-field of the B operand.

  :SUB 

  [op op-location active-process player game]
  (let [{:keys [a-target b-target game a-immediate]} (normalise-op op-location game player)

        new-game (if a-immediate 
                   (update-in game [:core b-target :b :target] #(- % a-immediate))
                   (let [a-a (get-in game [:core a-target :a :target])
                         a-b (get-in game [:core a-target :b :target])]
                     (-> game
                         (update-in [:core b-target :a :target] #(- % a-a))
                         (update-in [:core b-target :b :target] #(- % a-b)))))]

    (-> new-game
        (mark-field-ownership player b-target)
        (jump player active-process (inc op-location)))))

(defmethod execute-op 

  ;;  MOV  A  B  :  If  the   A  operand  is
  ;;  immediate it  is placed in the B-field
  ;;  of the  memory  location  specified by
  ;;  the B  operand, otherwise the contents
  ;;  of   the    entire   memory   location
  ;;  specified by the A operand is moved to
  ;;  the memory location specified by the B
  ;;  operand.

  :MOV 

  [op op-location active-process player game]
  (let [{:keys [a-target b-target game a-immediate]} (normalise-op op-location game player)
        op (get-in game [:core op-location])

        new-game (if a-immediate 
                   (assoc-in game [:core b-target :b :target] a-immediate)                     
                   (assoc-in game [:core b-target] (get-in game [:core a-target])))]

    (-> new-game
        (mark-field-ownership player b-target)
        (jump player active-process (inc op-location)))))

(defmethod execute-op 

  ;; JMP A  B :  The address  of the memory
  ;; location specified by the A operand is
  ;; placed  at  the  back  of  the process
  ;; queue  associated  with  the executing
  ;; program.   The   B  operand  does  not
  ;; necessarily    participate    in   the
  ;; execution of the instruction.

  :JMP
 
  [op op-location active-process player game]
  (let [{:keys [a-target b-target game]} (normalise-op op-location game player)
        op (get-in game [:core op-location])]

    (jump game player active-process a-target)))

(defmethod execute-op 

  ;; JMZ  A  B  :  If  the B-field of the B
  ;; operand is zero  then  the  address of
  ;; the memory location specified by the A
  ;; operand is placed at  the back  of the
  ;; process  queue   associated  with  the
  ;; executing program.

  :JMZ
 
  [op op-location active-process player game]
  (let [{:keys [a-target b-target game]} (normalise-op op-location game player)
        op (get-in game [:core op-location])]

    (jump game player active-process (if (= 0 (get-in game [:core b-target :b :target]))
                                       a-target 
                                       (inc op-location)))))

(defmethod execute-op 
  
  ;; JMN A B :  If  the  B-field  of  the B
  ;; operand is  not zero  then the address
  ;; of the  memory  location  specifiec by
  ;; the A operand is placed at the back of
  ;; the process queue associated  with the
  ;; executing program.

  :JMN 
  
  [op op-location active-process player game]
  (let [{:keys [a-target b-target game]} (normalise-op op-location game player)
        op (get-in game [:core op-location])]

    (jump game player active-process (if (not= 0 (get-in game [:core b-target :b :target]))
                                       a-target 
                                       (inc op-location)))))

(defmethod execute-op 
  
  ;; CMP  A   B  :  If  the  A  operand  is
  ;; immediate it  is  compared  to  the B-
  ;; field of the memory location specified
  ;; by  the  B   operand,   otherwise  the
  ;; contents of the entire memory location
  ;; specified by the A operand is compared
  ;; to the contents of the memory location
  ;; specified by  the  B  operand.  If the
  ;; compared  values  are  equal, the next
  ;; instruction  is  skipped  (the program
  ;; counter is incremented).
  
  :CMP 
  
  [op op-location active-process player game]
  (let [{:keys [a-target b-target game a-immediate]} (normalise-op op-location game player)
        op (get-in game [:core op-location])
        game (if-not a-immediate (mark-field-ownership game player a-target) game)]

    (-> game 
        (mark-field-ownership player b-target)        
        (jump player active-process 
              (if a-immediate
                (if (= (get-in game [:core b-target :b :target])
                       a-immediate) 
                  (+ op-location 2)
                  (+ op-location 1))
                (if (= (get-in game [:core b-target])
                       (get-in game [:core a-target]))
                  (+ op-location 2)
                  (+ op-location 1)))))))

(defmethod execute-op 
  
  ;; SLT  A  B  :  If  the A operand is not
  ;; immediate, the B-field  at  the memory
  ;; location specified by the A operand is
  ;; compared  to  the  B-field  of  the  B
  ;; operand,   otherwise   the  A  operand
  ;; itself is  used in  the comparison. If
  ;; the A  value is less than the B value,
  ;; the next instruction  is  skipped (the
  ;; program counter is incremented).
  
  :SLT 
  
  [op op-location active-process player game]
  (let [{:keys [a-target b-target game a-immediate]} (normalise-op op-location game player)
        op (get-in game [:core op-location])]

    (jump game player active-process 
          (if a-immediate
            (if (> (get-in game [:core b-target :b :target])
                   a-immediate) 
              (+ op-location 2)
              (+ op-location 1))
            (if (> (get-in game [:core b-target :b :target])
                   (get-in game [:core a-target :b :target]))
              (+ op-location 2)
              (+ op-location 1))))))

(defmethod execute-op 
  
  ;; DJN  A  B  :  If  the B operand is not
  ;; immediate, the B-field  of  the memory
  ;; location specified by the B operand is
  ;; fetched,    decremented,    and   then
  ;; restored, otherwise the B-field of the
  ;; current instruction  is  used.  If the
  ;; value is  not zero, the address of the
  ;; memory location specified by the  A operand is   
  ;; placed at the back of the process queue   
  ;; associated with the executing program.

  :DJN 
  
  [op op-location active-process player game]
  (let [{:keys [a-target b-target b-immediate game]} (normalise-op op-location game player)
        op (get-in game [:core op-location])
        game (-> game 
                 (mark-field-ownership player b-target)
                 (update-in [:core b-target :b :target] dec))]
    
    (jump game player active-process (if (= 0 (get-in game [:core b-target :b :target]))
                                       (inc op-location)
                                       a-target))))

(defmethod execute-op 

  ;; SPL A B : After a process has caused an
  ;; SPL  instruction  to  be  fetched,  the
  ;; program  counter   is  incremented  and
  ;; placed  at  the  back  of  its  process
  ;; queue.  The  address  ot   the  spawned
  ;; process is then placed  at the  back of
  ;; the same queue, providing  the queue is
  ;; not  full.  The  B   operand  does  not
  ;; necessarily    participate    in    the
  ;; execution of this instruction.         

  :SPL
 
  [op op-location active-process player game]
  (let [{:keys [a-target b-target game]} (normalise-op op-location game player)
        op (get-in game [:core op-location])
        ;; First do jump so that instruction just behind 
        ;; it executes sooner than the one we've split to.
        game (jump game player active-process (inc op-location))]
    (update-in game [player :processes] #(conj % a-target))))


(defmethod execute-op 
  
  ;; DAT  A  B  : This instruction actually
  ;; serves two functions, one utilitarian,
  ;; and  the  other  quite destructive. On
  ;; the  one  hand,  it  is  used  for the
  ;; storage  of  data  to  be  used  as  a
  ;; counter,  pointer,  etc.  If  executed
  ;; however, the  process is halted! This,
  ;; then, is the  mechanism  by  which one
  ;; obtains victory  in a  Core War: cause
  ;; every  process   associated  with  the
  ;; opponent's  program  to  execute a DAT
  ;; instruction.
  
  :DAT 
  
  [op op-location active-process player game]
  ;; (println "KK" (keys game))
  (let [{a-target :a-target b-target :b-target game-after :game} (normalise-op op-location game player)
        op (get-in game-after [:core op-location])
        processes (get-in game-after [player :processes])]
    (if (< (count processes) 2)
      (merge game 
             {:game-over true
              :loser player})
      (-> game-after 
          (assoc-in [player :active-process] (mod (inc active-process) (dec (count processes))))
          (assoc-in [player :processes] (vec (concat (subvec processes 0 active-process) 
                                                     (subvec processes (inc active-process) (count processes)))))))))


(defmethod execute-op :default [op op-location active-process player game]   
  (assert false (str "Unsupported op: '" op "' " op-location " " active-process " " player " game: " game)))


(defn game-step [game]
  (assert (not (:game-over game)))
  (let [idx (get-in game [:wolf :active-process])
        op-idx (get-in game [:wolf :processes idx])
        wolf-op (get-in game [:core op-idx])
        game-wolf (execute-op wolf-op op-idx idx :wolf game)

        idx (get-in game-wolf [:cock :active-process])
        op-idx (get-in game-wolf [:cock :processes idx])
        cock-op (get-in game-wolf [:core op-idx])

        game-cock (execute-op cock-op op-idx idx :cock game-wolf)]
    game-cock))

(defn play-game [game iterations-before-draw]    
  (loop [round 0 g game]
    (let [game-after (game-step g)]
      (cond (>= round iterations-before-draw) (assoc game-after :game-over true)
            (:game-over game-after) game-after
            :default (recur (inc round) game-after)))))
