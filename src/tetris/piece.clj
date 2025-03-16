(ns tetris.piece
  (:require [tetris.shape :as shape]
            [tetris.position :as pos]
            [tetris.graphical :as graphical]))

;; piece
(defn make-piece [shape pos] {:shape shape, :pos pos})
(defn piece-shape [piece] (get piece :shape))
(defn piece-pos [piece] (get piece :pos))

(defn piece-shape-set! [piece shape]
  (make-piece shape (piece-pos piece)))
(defn piece-pos-set! [piece pos]
  (make-piece (piece-shape piece) pos))

(defn piece->pos-ls [piece]
  (let [pos (piece-pos piece),
        pos-ls (shape/shape-pos-ls (piece-shape piece))]
    (map (fn [p] (pos/pos-add p pos)) pos-ls)))

;; shape->board
(defn piece->board [piece init-board]
     (let [pos-ls (piece->pos-ls piece)]
       (loop [board init-board, pos-ls pos-ls]
         (if (empty? pos-ls)
           board
           (let [pos (first pos-ls),
                 new-board (graphical/add-block board (first pos) (last pos))]
             (recur new-board (rest pos-ls)))))))
