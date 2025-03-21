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
  (reduce (fn [board pos]
            (graphical/add-block board
                                 (pos/x pos)
                                 (pos/y pos)))
          init-board
          (->pos-ls piece)))

(defn flip
  "Flips a piece 90' anti-clockwise"
  [piece]
  (let [old-shape (shape piece)
        new-shape (shape/flip (shape piece))
        old-height (shape/height old-shape)
        old-width (shape/width old-shape)
        new-height (shape/height new-shape)
        new-width (shape/width new-shape)
        old-pos (pos piece)
        width-diff (- old-width new-width)
        height-diff (- old-height new-height)
        shift (pos/make width-diff height-diff)
        new-pos (pos/add old-pos shift)]
    (make new-shape new-pos)))


(comment
  (def p (make shape/l (pos/make 0 0)))
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 0)}

  (let [piece p
        old-shape (shape piece)
        new-shape (shape/flip (shape piece))
        old-height (shape/height old-shape)
        old-width (shape/width old-shape)
        new-height (shape/height new-shape)
        new-width (shape/width new-shape)
        old-pos (pos piece)
        width-diff (dec (- old-width new-width))
        height-diff (dec (- old-height new-height))
        shift (pos/make width-diff height-diff)
        new-pos (pos/sub old-pos shift)]
    new-pos)
  ;; => (2 0)
  ;; => (1 -1)

  (flip p)
  ;; => {:shape {:pos-ls ((0 0) (1 0) (2 0) (2 1))}, :pos (-2 0)}
  ;; => {:shape {:pos-ls ((0 0) (1 0) (2 0) (2 1))}, :pos (-1 1)}
  ;; => {:shape {:pos-ls ((0 0) (1 0) (2 0) (2 1))}, :pos (-1 1)}
  ;; => {:shape {:pos-ls ((0 0) (1 0) (2 0) (2 1))}, :pos (1 -1)}
  ;; => {:shape {:pos-ls ((0 0) (1 0) (2 0) (2 1))}, :pos (1 0)}
  ;; => {:shape {:pos-ls ((0 0) (1 0) (2 0) (2 1))}, :pos (0 -1)}
  ;; => {:shape {:pos-ls ((0 0) (1 0) (2 0) (2 1))}, :pos (0 -1)}
  ;; => {:shape {:pos-ls ((0 0) (1 0) (2 0) (2 1))}, :pos (1 -1)}
  ;; => Syntax error compiling at (src/tetris/piece.clj:50:3).
  ;;    Unable to resolve symbol: p in this context
  ;; => {:shape {:pos-ls ((0 0) (1 0) (2 0) (2 1))}, :pos (0 0)}
  )
