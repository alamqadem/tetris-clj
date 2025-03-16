(ns tetris.graphical
  (:gen-class)
  (:require [clojure.string :as str]))

;; graphical system
(defn row [n]
  (into [] (repeat n "   ")))

(defn make-matrix [n]
  (into []
        (repeat n
                (row n))))

(defn -make [matrix size]
      {:board matrix, :size size})

(defn make [n]
      (-make (make-matrix n) n))

(defn matrix [board]
      (board :board))

(defn size [board]
      (get board :size))

(defn size-set! [board size]
      (-make (matrix board) size))

(defn matrix-set! [board board-values]
      (-make board-values (size board)))


(defn to-str [board]
  (reduce #(str %1 "\n" %2)
          (map #(reduce str %1) (matrix board))))

(defn print-board [board]
  (let [size (size board)]
    (println (reduce #(str %1 %2) (repeat size   "---")))
    (println (to-str board))
    (println (reduce #(str %1 %2) (repeat size  "---")))))

(defn pos-in-board? [pos-x pos-y board]
  (and
   (>= pos-x 0)
   (< pos-x (size board))
   (>= pos-y 0)
   (< pos-y (size board))))

(defn add-block [board pos-x pos-y]
  (if (not (pos-in-board? pos-x pos-y board))
    (throw (ex-info
            "attempt to write in position outside of the board"
            {:pos (list pos-x pos-y) :size (size board)})))
  (let [board-values (matrix board)
        board-row (get board-values pos-y)
        updated-board-row (assoc board-row pos-x "[ ]")
        new-board-values  (assoc board-values pos-y updated-board-row)]
    (matrix-set! board new-board-values)))

(defn add-rand-block [board]
  (add-block board (rand-int (size board)) 0))

(defn move-board [board]
      (let [size (size board)
        board-values (matrix board)
        new-board-values
          (into []
                (concat [(row size)]
                        (take (- size 1) board-values)))]
           (matrix-set! board new-board-values)))

(comment
  (repeat 3 "[ ]")
  ;; => ("[ ]" "[ ]" "[ ]")
  (repeat 4 (repeat 4 " "))
  ;; => ((" " " " " " " ")
  ;;     (" " " " " " " ")
  ;;     (" " " " " " " ")
  ;;     (" " " " " " " "))
  (make 5)
  ;; => ((" " " " " " " " " ")
  ;;     (" " " " " " " " " ")
  ;;     (" " " " " " " " " ")
  ;;     (" " " " " " " " " ")
  ;;     (" " " " " " " " " "))
  (def board (make 5))
  ;; => #'tetris.core/board
  (assoc (into [] board) 0
         (assoc (into [] (repeat 5 " ")) 0 "[ ]"))
  ;; => [["[ ]" " " " " " " " "]
  ;;     (" " " " " " " " " ")
  ;;     (" " " " " " " " " ")
  ;;     (" " " " " " " " " ")
  ;;     (" " " " " " " " " ")]
  (defn row [n] (into [] (repeat n "   ")))
  ;; => #'tetris.core/row
  (row 3)
  ;; => ["   " "   " "   "]
  (make 4)
  ;; => [["   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   "]]
  (get board 3)
  ;; => ["   " "   " "   " "   " "   "]
  (defn add-block [board pos-x pos-y]
    (assoc board pos-y
           (assoc (get board pos-y) pos-x "[ ]")))
  (add-block board 1 1)
  ;; => [["   " "   " "   " "   " "   "]
  ;;     ["   " "[ ]" "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]]

  ;; how can I print the board? 🤔

  (str (get board 0))

  (def new-board (add-block board 1 1))

  (for [x (range 5)]
    (let [row (get new-board x)]
      (for [y (range 5)]
        (do
          (print (str x ", " y))
          (print (get row y))))
      (print "\n")))

  ;; board to string
  (reduce #(str %1 "\n" %2)
          (map #(reduce str %1) new-board))

  (clojure.string/join "\n"
                       (map #(reduce str %) new-board))

  ;; print board
  (print (to-str new-board))

  ;; add a new block at a random position
  (do
    (print-board  (add-block new-board (rand-int 5) 0))
    (print "\n"))
  (add-block new-board (rand-int 5) 0)
  ;; => [["   " "   " "   " "   " "[ ]"]
  ;;     ["   " "[ ]" "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]]

  (def board (make 5))
  (get (matrix board) 5)
  ;; => nil
  (assoc nil 0 "[ ]")
  ;; nil is considered as the empty map, treat 0 as the key
  ;; and "[ ]" as the value
  ;; => {0 "[ ]"}
  ;; move blocks by one
  (concat [(row 5)] (take 4 new-board))
  ;; => (["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "[ ]" "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "])
  (move-board new-board)
  ;; => (["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "[ ]" "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "])
  (add-rand-block new-board)
  ;; => [["   " "   " "   " "[ ]" "   "]
  ;;     ["   " "[ ]" "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   "]]
  (def board-with-new-block (add-rand-block new-board))
  board-with-new-block

  (def moved-board (move-board board-with-new-block))
  moved-board
  (concat [(row 5)] (take 4 board-with-new-block))

  (let [board-with-new-block (add-rand-block new-board)
        moved-board (move-board board-with-new-block)]
    (print-board moved-board))

  (-make (make-matrix 3) 3)
  ;; => {:board
  ;;     [["   " "   " "   "] ["   " "   " "   "] ["   " "   " "   "]],
  ;;     :size 3}

  (move-board (add-rand-block new-board))
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "[ ]" "   "]
  ;;      ["   " "[ ]" "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]],
  ;;     :size 5})

    ;; testing that drawing a shape works as expected in graphical
  ;; draw-shape
  (def board-with-a-shape
    ((fn [shape board]
       (let [pos-ls (get shape :pos-ls)]
         (loop [current-board board, current-pos-ls pos-ls]
           (if (empty? current-pos-ls)
             current-board
             (let [current-pos (first current-pos-ls),
                   new-board (add-block current-board (first current-pos) (last current-pos))]
               (recur new-board (rest current-pos-ls)))))))
     {:pos-ls [(list 0 0) (list 0 1) (list 1 0) (list 2 0)]} (make 20)))
  ;; => {:board
  ;;     [["[ ]" "[ ]" "[ ]" "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]],
  (print-board board-with-a-shape))
