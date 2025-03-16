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

(defn board-get [board x y]
  (let [board-values (matrix board)]
    (get (get board-values y) x)))

(defn to-str [board]
      (reduce #(str %1 "\n" %2)
              (map #(reduce str %1) (matrix board))))

(defn print-board [board]
  (let [size (size board)]
    (println (reduce #(str %1 %2) (repeat size   "---")))
    (println (to-str board))
    (println (reduce #(str %1 %2) (repeat size  "---")))))

(defn add-block [board pos-x pos-y]
      (let [board-values (matrix board)
        new-board-values  (assoc board-values pos-y
                            (assoc (get board-values pos-y) pos-x "[ ]"))]
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
  (assoc (into [ ] board) 0
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
)
