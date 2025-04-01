(ns tetris.movement
  (:require [tetris.position :as pos]
            [tetris.shape :as shape]
            [tetris.piece :as piece]))


(defn make
  "Create a new movement, move-fn is a function that takes a piece and produces the moved piece"
  [move-fn]
  {:move move-fn})

(defn move-fn
  "Get movement function from a movement, internal use only"
  [movement]
  (:move movement))

(defn move
  "Apply movement to piece and get the moved piece"
  [piece movement]
  ((move-fn movement) piece))

(def no-movement (make identity))

(defn combine
  "Given a variadic number of movements, return a movement that applied"
  [& movements]
  (let [combine2
        (fn [movement1 movement2]
          (make (comp (move-fn movement2) (move-fn movement1))))]
    (reduce combine2 no-movement movements)))

(defn make-from-direction
  [direction]
  (make
   (fn [piece]
     (piece/make
      (piece/shape piece)
      (pos/add (piece/pos piece) direction)))))

;; predefined movements
(def move-right (make-from-direction (pos/make 1 0)))

(def move-left (make-from-direction (pos/make -1 0)))

(def move-down (make-from-direction (pos/make 0 1)))

(def flip-move (make piece/flip))


(comment

  #{:move (fn [piece] piece)}

  (def piece (piece/make shape/l (pos/make 0 0)))
  piece
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 0)}

  (def piece-moved-right (piece/make
                          (piece/shape piece)
                          (pos/add (piece/pos piece) (pos/make 1 0))))

  piece-moved-right
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (1 0)}
  ;; move right
  (def move-right
    (make
     (fn [piece]
       (piece/make
        (piece/shape piece)
        (pos/add (piece/pos piece) (pos/make 1 0))))))
  move-right
  ;; => {:move #function[tetris.movement/eval8787/fn--8788]}

  (def piece-moved-right (move piece move-right))
  piece-moved-right
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (1 0)}

  (def piece-moved-right (move piece move-right))
  piece-moved-right
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (1 0)}

  (move piece move-left)
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (-1 0)}

  (move piece move-down)
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 1)}

  (move piece no-movement)
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 0)}

  ;; seeing how to combine 2 functions
  inc
  (inc 1)
  ;; => 2

  (dec (inc 1))
  ;; => 1

  ((comp dec inc) 1)
  ;; => 1

  (-> 1 inc dec)
  ;; => 1
  ((fn [i] (-> i inc dec)) 1)
  ;; => 1
  (#(-> %1 inc dec) 1)
  ;; => 1
  ((-> i inc dec) 1)
  ;; => Execution error (ClassCastException) at tetris.movement/eval8493 (form-init6066971824390677389.clj:33).
  ;;    class clojure.core$inc cannot be cast to class java.lang.Number (clojure.core$inc is in unnamed module of loader 'app'; java.lang.Number is in module java.base of loader 'bootstrap')

  ;; only comp for now, I was trying to see if there is something similar to -> where I can see the order of
  ;; the functions to combine them
  (def move-right-then-left (combine move-left move-right))

  (move piece move-right-then-left)
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 0)}

  (move piece (combine move-right move-left move-down))
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 1)}

  (move piece flip-move)
  ;; => {:shape {:pos-ls ((2 0) (1 0) (0 0) (0 1))}, :pos (-1 1)}

  ;; combine with only 1 argument works
  (move piece (combine move-right))
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (1 0)}
  ;; combine with no argument throws an error, because I pass the first element as the seed and the rest as the list to process
  ;; this could be a problem if I have a list of movements and I try to do (apply combine)
  ;; I could make it return for example the no-movement
  (move piece (combine))
  ;; => Execution error (NullPointerException) at tetris.movement/move (movement.clj:17).
  ;;    Cannot invoke "clojure.lang.IFn.invoke(Object)" because the return value of "clojure.lang.IFn.invoke(Object)" is null

  ;; ok I fixed it, by passing to reduce no-movement as the seed, it seems the most logical thing
  ;; much better *.*
  (move piece (combine))
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 0)}

  ;; flip combines fine as well
  (move piece (combine flip-move move-down))
  ;; => {:shape {:pos-ls ((2 0) (1 0) (0 0) (0 1))}, :pos (-1 2)}

  identity

  (true? (rest '()))
  ;; => false
  ;; end comment, don't write below this line :P
  )

