(ns tetris.graphical
  (:gen-class)
  (:require [clojure.string :as str]
            [tetris.position :as pos]))


;; graphical system
(def EMPTY-SPACE "   ")
(def BLOCK "[ ]")
(def BORDER "---")
(def POS-REGEX #"\s\s\s|\[\s\]")

(defn row [n]
  (into [] (repeat n EMPTY-SPACE)))

(defn make-matrix
  ([n]
   (make-matrix n n))
  ([width height]
   (into []
         (repeat height
                 (row width)))))

(defn -make
  ([matrix width height]
   (-make matrix (list width height)))
  ([matrix size]
   {:board matrix,
    :size size}))

(defn make
  ([n]
   (make n n))
  ([width height]
   (-make (make-matrix width height) width height)))

(defn matrix [board]
      (board :board))

(defn size [board]
      (get board :size))

(defn width [board]
  (first (size board)))

(defn height [board]
  (second (size board)))

(defn size-set!
  ([board width height]
   (size-set! board (list width height)))
  ([board size]
   (-make (matrix board) size)))

(defn matrix-set! [board board-values]
      (-make board-values (size board)))

(defn to-str [board]
  (let [size (width board)
        board-content (reduce #(str %1 "\n" %2)
                              (map #(apply str %1) (matrix board)))
        board-border (apply str (repeat size BORDER))]
    (str board-border "\n"
         board-content "\n"
         board-border)))

(defn from-str-ls
  "create a board given its string representation as a list of lines"
  [board-str-lines]
  (let [board-lines-without-border (drop-last 1 (drop 1 board-str-lines))
        lines-splitted (map (partial re-seq POS-REGEX)
                            board-lines-without-border)
        matrix (into [] (map (partial into []) lines-splitted))
        width (count (get matrix 0))
        height (count matrix)]
    (-make matrix width height)))

(defn from-str
  "create a board given its string representation"
  [board-as-str]
  (from-str-ls (str/split-lines board-as-str)))

(defn print-board [board]
  (println (to-str board)))

(defn pos-in-board? [pos-x pos-y board]
  (and
   (>= pos-x 0)
   (< pos-x (width board))
   (>= pos-y 0)
   (< pos-y (height board))))

(defn add-block [board pos-x pos-y]
  (if (not (pos-in-board? pos-x pos-y board))
    (throw (ex-info
            "attempt to write in position outside of the board"
            {:pos (list pos-x pos-y) :size (size board)})))
  (let [board-values (matrix board)
        board-row (get board-values pos-y)
        updated-board-row (assoc board-row pos-x BLOCK)
        new-board-values  (assoc board-values pos-y updated-board-row)]
    (matrix-set! board new-board-values)))

(defn get-block [board x y]
  (get (get (matrix board) y) x))

(defn has-block?
  "Returns true if the board has a block in position x,y"
  ([board pos]
   (has-block? board (pos/x pos) (pos/y pos)))
  ([board x y]
   (= (get-block board x y) BLOCK)))

(defn ->pos-ls
  "Returns the list of positions that have a block in the board"
  [board]
  (let [pos-ls (for [x (range (width board)), y (range (height board))] (pos/make x y))
        board-has-block? (partial has-block? board)]
    (filter board-has-block? pos-ls)))

(defn add-rand-block [board]
  (add-block board (rand-int (width board)) 0))

(defn move-board [board]
  (let [board-values (matrix board)
        new-board-values
        (into []
              (concat [(row (width board))]
                      (take (- (height board) 1) board-values)))]
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
  (print-board board-with-a-shape)

  (def board
    (-make
     [["   ", "[ ]", "[ ]", "[ ]", "   "]
      ["   ", "   ", "[ ]", "   ", "   "]
      ["   ", "   ", "   ", "   ", "[ ]"]
      ["   ", "   ", "   ", "   ", "[ ]"]
      ["   ", "   ", "   ", "[ ]", "[ ]"]]
     5))

  board
  ;; => {:board
  ;;     [["   " "[ ]" "[ ]" "[ ]" "   "]
  ;;      ["   " "   " "[ ]" "   " "   "]
  ;;      ["   " "   " "   " "   " "[ ]"]
  ;;      ["   " "   " "   " "   " "[ ]"]
  ;;      ["   " "   " "   " "[ ]" "[ ]"]],
  ;;     :size 5}

  (def pos-ls (->>
               (let [size-range (range (size board))]
                 (for [x size-range, y size-range]
                   (pos/make x y)))
               (filter (fn [pos]
                         (has-block? board pos)))))
  pos-ls
  ;; => ((1 0) (2 0) (2 1) (3 0) (3 4) (4 2) (4 3) (4 4))
  (defn long-str [& strings]
    (str/join "\n" strings))

  (def board-as-str
    (long-str
     "------------------------------"
     "                              "
     "                              "
     "                              "
     "                              "
     "                              "
     "                              "
     "                              "
     "                              "
     "                  [ ][ ][ ][ ]"
     "                              "
     "------------------------------"))

  (println board-as-str)

  (def board-as-lines (str/split-lines board-as-str))
  board-as-lines
  ;; => ["------------------------------"
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                  [ ][ ][ ][ ]"
  ;;     "                              "
  ;;     "------------------------------"]

  ;; split every 3 character (I should probably put this number somewhere)
  (re-seq #"\s\s\s|\[\s\]" (board-as-lines 1))
  ;; => ("   " "   " "   " "   " "   " "   " "   " "   " "   " "   ")
  (re-seq #"\s\s\s|\[\s\]" (board-as-lines 9))
  ;; => ("   " "   " "   " "   " "   " "   " "[ ]" "[ ]" "[ ]" "[ ]")
  ;; skip the first and the last lines
  (def board-lines-without-border (drop-last 1 (drop 1 board-as-lines)))
  board-lines-without-border
  ;; => ("                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                              "
  ;;     "                  [ ][ ][ ][ ]"
  ;;     "                              ")

  (def lines-splitted (map (partial re-seq #"\s\s\s|\[\s\]") board-lines-without-border))
  lines-splitted
  ;; => (("   " "   " "   " "   " "   " "   " "   " "   " "   " "   ")
  ;;     ("   " "   " "   " "   " "   " "   " "   " "   " "   " "   ")
  ;;     ("   " "   " "   " "   " "   " "   " "   " "   " "   " "   ")
  ;;     ("   " "   " "   " "   " "   " "   " "   " "   " "   " "   ")
  ;;     ("   " "   " "   " "   " "   " "   " "   " "   " "   " "   ")
  ;;     ("   " "   " "   " "   " "   " "   " "   " "   " "   " "   ")
  ;;     ("   " "   " "   " "   " "   " "   " "   " "   " "   " "   ")
  ;;     ("   " "   " "   " "   " "   " "   " "   " "   " "   " "   ")
  ;;     ("   " "   " "   " "   " "   " "   " "[ ]" "[ ]" "[ ]" "[ ]")
  ;;     ("   " "   " "   " "   " "   " "   " "   " "   " "   " "   "))
  (def matrix (into [] (map (partial into []) lines-splitted)))
  matrix
  ;; => [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;     ["   " "   " "   " "   " "   " "   " "[ ]" "[ ]" "[ ]" "[ ]"]
  ;;     ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]]
  (count matrix)
  p  ;; => 10
  (count (matrix 0))
  ;; => 10
  ;; let's put everything in a board
  (let [board-lines-without-border (drop-last 1 (drop 1 board-as-lines))
        lines-splitted (map (partial re-seq #"\s\s\s|\[\s\]") board-lines-without-border)
        matrix (into [] (map (partial into []) lines-splitted))]
    (-make matrix (count matrix)))
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "[ ]" "[ ]" "[ ]" "[ ]"]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}
  )

