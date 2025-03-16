(ns tetris.piece
  (:require [tetris.shape :as shape]
            [tetris.position :as pos]
            [tetris.graphical :as graphical]))

;; piece
(defn make [shape pos] {:shape shape, :pos pos})
(defn shape [piece] (get piece :shape))
(defn pos [piece] (get piece :pos))

(defn shape-set! [piece shape]
  (make shape (pos piece)))
(defn pos-set! [piece pos]
  (make (shape piece) pos))

(defn ->pos-ls [piece]
  (let [pos (pos piece),
        pos-ls (shape/pos-ls (shape piece))]
    (map (fn [p] (pos/add p pos)) pos-ls)))

;; shape->board
(defn ->board [piece init-board]
     (let [pos-ls (->pos-ls piece)]
       (loop [board init-board, pos-ls pos-ls]
         (if (empty? pos-ls)
           board
           (let [pos (first pos-ls),
                 new-board (graphical/add-block board
                                                (pos/x pos)
                                                (pos/y pos))]
             (recur new-board (rest pos-ls)))))))
