(ns tetris.game
  (:require [tetris.graphical :as graphical]
            [tetris.position :as pos]
            [tetris.shape :as shape]
            [tetris.piece :as piece]
            [tetris.movement :as movement]))

(defn make
  ([time pieces size]
   (make time pieces size 0))
  ([time pieces size points]
   {:time time,
    :pieces (seq pieces),
    :size size
    :points points}))

(defn game-time [game]
  (get game :time))

(defn pieces [game]
  (get game :pieces))

(defn size [game]
  (get game :size))

(defn bottom-right-pos
  [game]
  (let [s (dec (size game))]
    (pos/make s s)))

(defn points [game]
  (:points game))

(defn game-time-set! [game time]
  (make
   time
   (pieces game)
   (size game)
   (points game)))

(defn pieces-set! [game pieces]
  (make (game-time game) pieces (size game) (points game)))

(defn size-set! [game size]
  (make (game-time game) (pieces game) size (points game)))

(defn points-set! [game points]
  (make (game-time game) (pieces game) (size game) points))

(defn current-piece
  "given a game returns the current piece that is failling"
  [game]
  (first (pieces game)))

(defn rest-pieces
  "returns all the pieces except the current piece"
  [game]
  (rest (pieces game)))

(defn other-pieces
  "returns all the pieces that are not piece"
  [game piece]
  (filter (partial piece/not-eq? piece) (pieces game)))

(defn add-piece
  "adds a piece in such a way that the new piece is the current piece"
  [game piece]
  (pieces-set! game (cons piece (pieces game))))

(defn ->board
  "transforms a game into a board"
  [game]
  (let [pieces (pieces game)
        board (graphical/make (size game))]
    (reduce (fn [board p] (piece/->board p board)) board pieces)))

(defn to-str
  "returns the string representation for a game"
  [game]
  (graphical/to-str (->board game)))

(defn to-str-status
  "returns the string representing the status of a game"
  [game]
  (let [game-info (str (game-time game))
        status-length (count game-info)
        spaces (apply str (repeat (- (* (size game) 3) status-length) " "))
        status-bar (str spaces game-info)]
    status-bar))

(defn print-game
  "prints a game on the output including the status"
  [game]
  (println (to-str game))
  (println (to-str-status game)))

(defn ->game
  "Given a board, it calculates the corresponding game"
  [board]
  (let [pos-ls (graphical/->pos-ls board)
        groups (pos/find-contiguous pos-ls)
        pos-ls (map pos/min-pos groups)
        shapes (map shape/make groups)
        norm-shapes (map shape/normalize shapes pos-ls)
        pieces (map piece/make norm-shapes pos-ls)]
    (make 0 pieces (graphical/size board))))

(defn outside-of-boundaries?
  "returns true if moving piece in the game to pos makes it outside of the game boundaries"
  ([game piece pos]
   (let [piece-after-move (piece/pos-set! piece pos)]
     (outside-of-boundaries? game piece-after-move)))
  ([game piece]
   (not (piece/within-boundaries? piece (bottom-right-pos game)))))

(defn collision-with-other-piece?
  "returns true if piece moved to pos collides with another piece"
  ([game piece pos]
   (let [other-pieces-ls (other-pieces game piece)
         piece-after-move (piece/pos-set! piece pos)]
     (collision-with-other-piece? piece-after-move other-pieces-ls)))
  ([piece-after-move other-pieces-ls]
   (some (partial piece/collision? piece-after-move) other-pieces-ls)))

(defn can-move?
  "true if it can move a piece in game to pos, false otherwise"
  ([game movement]
   (let [piece (current-piece game)]
     (can-move? game piece movement)))
  ([game piece movement]
   (let [other-pieces-ls (other-pieces game piece)
         piece-after-move (movement/move piece movement)]
     (not (or
           (outside-of-boundaries? game piece-after-move)
           (collision-with-other-piece? piece-after-move other-pieces-ls))))))

(defn move-piece
  ([game movement]
   (let [piece (current-piece game)]
     (move-piece game piece movement)))
  ([game piece movement]
   (let [piece-after-move (movement/move piece movement)
         game-without-piece (pieces-set! game (other-pieces game piece))]
     (add-piece game-without-piece piece-after-move))))

(defn add-random-piece [game]
  ;;adds a new piece of a random shape at the top of the game in a random position
  (let [rand-shape (shape/random-shape)
        rand-shape-width (shape/width rand-shape)
        rand-pos (pos/rand-pos (- (size game) (dec rand-shape-width)))
        rand-piece (piece/make rand-shape rand-pos)]
    (add-piece game rand-piece)))

(defn row-is-full?
  "Checks if the given row index is full"
  [game row-index]
  (let [pieces-ls (pieces game)
        all-pos-ls (mapcat piece/->pos-ls pieces-ls)
        pos-ls-in-row (filter (fn [p] (= (pos/y p) row-index)) all-pos-ls)]
    (= (count pos-ls-in-row) (size game))))

(defn full-rows
  "Returns a list of all the row indexes that are full"
  [game]
  (filter (partial row-is-full? game) (range 0 (size game))))

(defn remove-full-rows
  "Returns a game with all the full-rows removed"
  [game full-rows]
  (let [remove-full-rows-fn (fn [p] (piece/remove-rows p full-rows))
        pieces-without-full-rows (map remove-full-rows-fn (pieces game))
        not-empty-piece? (comp not empty? piece/->pos-ls)
        pieces-that-are-not-empty (filter not-empty-piece?
                                          pieces-without-full-rows)
        game-updated (pieces-set! game pieces-that-are-not-empty)]
    game-updated))

(defn move-all-pieces-down
  [game]
  (let [can-move-piece? (fn [piece] (can-move? game piece movement/move-down))
        pieces-that-can-be-moved (filter can-move-piece? (pieces game))
        move-piece (fn [game piece] (move-piece game piece movement/move-down))]
    (if (empty? pieces-that-can-be-moved)
      game
      (recur (reduce move-piece game pieces-that-can-be-moved)))))

(defn game-over? [game]
  (if (empty? (pieces game))
      false
      (and
         (= (-> (current-piece game) piece/pos pos/y) 0)
         (not (can-move? game movement/move-down)))))

(defn update-game
  ([game]
   (update-game game movement/no-movement))
  ([game movement]
   (let [game-updated (game-time-set! game (inc (game-time game)))
         piece (current-piece game-updated)]
     ;; increment time
     (if (not piece)
       (add-random-piece game-updated)
       (let [move-down1 movement/move-down
             ;; apply movement left/right/flip
             game-updated (if (can-move? game-updated movement)
                            (move-piece game-updated movement)
                            game-updated)]
         ;; move current piece down by 1 if present
         ;; if the current piece cannot move add a new random piece
         (if (can-move? game-updated move-down1)
           (move-piece game-updated move-down1)
           (let [full-rows-ls (full-rows game-updated)
                 game-updated (remove-full-rows game-updated full-rows-ls)
                 game-updated (move-all-pieces-down game-updated)]
             (add-random-piece game-updated))))))))

(comment
  {:time 0,
   :pieces [nil, nil, nil],
   :size 5}

  (make 0 [] 10)
  ;; => {:time 0, :pieces [], :size 10}

  (pos-in-game? (make 0 [] 10) (pos/make 1 1))
  ;; => true

  (outside-of-boundaries? (make 0 [] 10) (piece/make shape/l (pos/make 1 1)))
  ;; => false
  (outside-of-boundaries? (make 0 [] 10) (piece/make shape/l (pos/make 1 10)))
  ;; => true
  (pos/in-pos-ls? (pos/make 0 1) [(pos/make 1 1) (pos/make 2 1) (pos/make 0 1)])
  ;; => true

  (pos/in-pos-ls? (pos/make 0 1) [(pos/make 1 1) (pos/make 2 1) (pos/make 0 0)])
  ;; => false

  (pos/pos-ls-intersect? [(pos/make 0 0) (pos/make 1 1)] [(pos/make 1 0) (pos/make 2 1)])
  ;; => false

  (pos/pos-ls-intersect? [(pos/make 0 0) (pos/make 1 1)] [(pos/make 1 0) (pos/make 1 1)])
  ;; => true
  (def game
    (make 0 [(piece/make shape/l (pos/make 1 1))
             (piece/make shape/block (pos/make 3 0))]
          10))

  (def piece (get (pieces game) 1))
  (def pos (pos/make 1 1))
  ;; (fold

  (def board (graphical/make 5))

  (let [piece (piece/make shape/block (pos/make 3 0))
        other-pieces (filter (fn [p] (not= p piece)) (pieces game))
        other-pieces-pos-ls (map piece/->pos-ls other-pieces)]
    other-pieces-pos-ls)
  ;; => (((1 1) (1 2) (2 1) (3 1)) ((3 1)))

  (loop [board1 board,
         pieces [(piece/make shape/l (pos/make 0 0))
                 (piece/make shape/block (pos/make 4 0))
                 (piece/make shape/block (pos/make 0 3))]]
    (if (empty? pieces)
      board1
      (recur (piece/->board (first pieces) board1)
             (rest pieces))))
  ;; => {:board
  ;;     [["[ ]" "[ ]" "[ ]" "   " "[ ]"]
  ;;      ["[ ]" "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]],
  ;;     :size 5}
  (let [pieces (pieces game)]
    (reduce (fn [board p] (piece/->board p board)) board pieces))
  ;; => {:board
  ;;     [["   " "   " "   " "[ ]" "   "]
  ;;      ["   " "[ ]" "[ ]" "[ ]" "   "]
  ;;      ["   " "[ ]" "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]],
  ;;     :size 5}
  (current-piece game)
  ;; => {:shape {:pos-ls [(0 0) (0 1) (1 0) (2 0)]}, :pos (1 1)}
  (add-piece game (piece/make shape/square (pos/make 0 3)))
  ;; => {:time 0,
  ;;     :pieces
  ;;     [{:shape {:pos-ls [(0 0) (0 1) (1 0) (2 0)]}, :pos (1 1)}
  ;;      {:shape {:pos-ls [(0 0)]}, :pos (3 0)}
  ;;      {:shape {:pos-ls [(0 0) (0 1) (1 0) (1 1)]}, :pos (0 3)}],
  ;;     :size 10}

  (def empty-game
    (make 0 [] 10))

  (->board empty-game)
     ;; => {:board
     ;;     [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
     ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
     ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
     ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
     ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
     ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
     ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
     ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
     ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
     ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]],
     ;;     :size 10}

  (->board (add-random-piece empty-game))
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "   " "[ ]" "[ ]" "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "[ ]" "[ ]" "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}

  ; let's see what happens if I take a vector and do conj on it
  (def first-conj (conj [] 1))
  ;; => [1]
  (last first-conj)
  ;; => 1
  (def second-conj (conj (filter (fn [p] true) first-conj) 2))
  ;; => (2 1)

  ;; => [1 2]
  (last second-conj)
  ;; => 2

  ;; conj seems to work, let's now try to reproduce the problem
  ;; after a block reaches the bottom a new piece is added, but then afterwards a new piece is added
  (def game-with-bug
    (make 0 [(piece/make shape/l (pos/make 0 7))] 10))

  (->board game-with-bug)
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}

  (def game-with-bug1
    (update-game game-with-bug))
  (->board game-with-bug1)
  ;; => {:board
  ;;     [["   " "   " "[ ]" "[ ]" "[ ]" "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "[ ]" "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}
  ;; => {:board
  ;;     [["   " "   " "[ ]" "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "[ ]" "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}

  (def game-with-bug2
    (update-game game-with-bug1))
  (->board game-with-bug2)
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "[ ]" "[ ]" "[ ]" "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "[ ]" "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "[ ]" "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "[ ]" "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}

  (def game-with-bug3
    (update-game game-with-bug2))
  (->board game-with-bug3)
  ;; => {:board
  ;;     [["   " "   " "   " "   " "[ ]" "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "[ ]" "[ ]" "[ ]" "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "[ ]" "[ ]" "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "[ ]" "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "[ ]" "[ ]" "   " "   "]
  ;;      ["   " "   " "[ ]" "   " "   " "   " "[ ]" "[ ]" "   " "   "]
  ;;      ["   " "   " "[ ]" "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}

  (current-piece game-with-bug2)
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)}
  ;; => {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)}
  (pieces game-with-bug2)
  ;; => ({:shape {:pos-ls [(0 0) (1 0) (2 0) (1 1)]}, :pos (2 1)}
  ;;     {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)})
  ;; => ({:shape {:pos-ls [(1 0) (1 1) (1 2) (0 2)]}, :pos (1 1)}
  ;;     {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)})

  (pieces game-with-bug1)
  ;; => [{:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)}
  ;;     {:shape {:pos-ls [(0 0) (1 0) (2 0) (1 1)]}, :pos (2 0)}]
  ;; => [{:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)}
  ;;     {:shape {:pos-ls [(1 0) (1 1) (1 2) (0 2)]}, :pos (1 0)}]

  (def game-test-collision
    (add-piece
     (make 0 [(piece/make shape/l (pos/make 0 2))] 5)
     (piece/make shape/square (pos/make 0 0))))

  (->board game-test-collision)
  ;; => {:board
  ;;     [["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]],
  ;;     :size 5}

  (def sq-piece (current-piece game-test-collision))
  ;; => {:shape {:pos-ls [(0 0) (0 1) (1 0) (1 1)]}, :pos (0 0)}

  (def new-pos
    (pos/add (piece/pos sq-piece) (pos/make 0 1)))

  new-pos
  ;; => (0 1)

  (collision-with-other-piece? game-test-collision sq-piece new-pos)
  ;; => nil

  (can-move? game-test-collision sq-piece movement/move-down)

  (def updated-game (update-game game-test-collision))

  (->board updated-game)
  ;; => {:board
  ;;     [["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]],
  ;;     :size 5}

  (def other-pieces (filter (fn [p] (not= p sq-piece)) (pieces game-test-collision)))
  other-pieces
  ;; => ({:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 2)})
  (def other-pieces-pos-ls (map piece/->pos-ls other-pieces))
  other-pieces-pos-ls
  ;; => (((0 2) (0 3) (0 4) (1 4)))
  (mapcat piece/->pos-ls other-pieces)
  (def piece-pos-ls (piece/->pos-ls sq-piece))
  piece-pos-ls
  ;; => ((0 0) (0 1) (1 0) (1 1))
  (def piece-pos-ls-after-move (map (fn [p] (pos/add p new-pos)) piece-pos-ls))
  piece-pos-ls-after-move
  ;; => ((0 1) (0 2) (1 1) (1 2))
  (pos/pos-ls-intersect? other-pieces-pos-ls piece-pos-ls-after-move)
  ;; => nil
  (pos/pos-ls-intersect? (reduce concat other-pieces-pos-ls) piece-pos-ls-after-move)
  ;; => true
  (def game-test-collision1
    (make 0 [(piece/make shape/l (pos/make 7 7))
             (piece/make shape/t (pos/make 6 5))] 10))
  (graphical/print-board (->board game-test-collision1))
  (def game-test-collision1-updated
    (update-game game-test-collision1))
  (graphical/print-board (->board game-test-collision1-updated))
  (def curr-piece (current-piece game-test-collision1))
  curr-piece
  ;; => {:shape {:pos-ls [(0 0) (1 0) (2 0) (1 1)]}, :pos (6 5)}
  (def new-pos (pos/add (pos/make 6 5) (pos/make 0 1)))
  new-pos
  ;; => (6 6)
  (can-move? game-test-collision1 curr-piece new-pos)
  (collision-with-other-piece? game-test-collision1 curr-piece new-pos)
  (def other-pieces (filter (fn [p] (not= p curr-piece)) (pieces game-test-collision1)))
  other-pieces
  ;; => ({:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (7 7)})
  (def other-pieces-pos-ls (mapcat piece/->pos-ls other-pieces))
  other-pieces-pos-ls
  ;; => ((7 7) (7 8) (7 9) (8 9))
  (def piece-pos-ls (piece/->pos-ls curr-piece))
  piece-pos-ls
  ;; => ((6 5) (7 5) (8 5) (7 6))
  (def piece-pos-ls-after-move (map (fn [p] (pos/add p new-pos)) piece-pos-ls))
  piece-pos-ls-after-move
  ;; => ((12 11) (13 11) (14 11) (13 12))
  (pos/pos-ls-intersect? other-pieces-pos-ls piece-pos-ls-after-move)
  ;; => nil

  (def piece-pos-ls-after-move (piece/->pos-ls (piece/pos-set! curr-piece new-pos)))
  piece-pos-ls-after-move
  ;; => ((6 6) (7 6) (8 6) (7 7))
  (pos/pos-ls-intersect? other-pieces-pos-ls piece-pos-ls-after-move)
  ;; => true

  (can-move? (make 0 [] 5) (piece/make shape/l (pos/make 0 2)) (movement/make-from-direction (pos/make -1 1)))
   ;; {:pos (-1 1), :size 10}

  (def board
    (graphical/-make
     [["   ", "[ ]", "[ ]", "[ ]", "   "]
      ["   ", "   ", "[ ]", "   ", "   "]
      ["   ", "   ", "   ", "   ", "[ ]"]
      ["   ", "   ", "   ", "   ", "[ ]"]
      ["   ", "   ", "   ", "[ ]", "[ ]"]]
     5))

  (->game board)
  ;; => {:time 0,
  ;;     :pieces
  ;;     ({:shape {:pos-ls ((0 2) (1 0) (1 1) (1 2))}, :pos (3 2)}
  ;;      {:shape {:pos-ls ((0 0) (1 0) (1 1) (2 0))}, :pos (1 0)}),
  ;;     :size 5}

  (to-str (->game board))
  ;;---------------\n
  ;;   [ ][ ][ ]   \n
  ;;      [ ]      \n
  ;;            [ ]\n
  ;;            [ ]\n
  ;;         [ ][ ]\n
  ;;---------------"

  (def board  (graphical/-make
               [["   ", "[ ]", "   ", "   ", "   "]
                ["   ", "[ ]", "[ ]", "   ", "   "]
                ["   ", "[ ]", "   ", "   ", "[ ]"]
                ["   ", "   ", "   ", "   ", "[ ]"]
                ["   ", "   ", "   ", "[ ]", "[ ]"]]
               5))

  (->game board)
  ;; => {:time 0,
  ;;     :pieces
  ;;     ({:shape {:pos-ls ((0 2) (1 0) (1 1) (1 2))}, :pos (3 2)}
  ;;      {:shape {:pos-ls ((0 0) (0 1) (0 2) (1 1))}, :pos (1 0)}),
  ;;     :size 5}

  (println (to-str (->game board)))
  ;; ---------------
  ;;    [ ]         
  ;;    [ ][ ]      
  ;;    [ ]      [ ]
  ;;             [ ]
  ;;          [ ][ ]
  ;; ---------------

  ;; calling update on this configuration tries to write outside of the
  ;; board
  (def game (->game (graphical/from-str-ls
                     ["------------------------------"
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
                      "------------------------------"])))

  game
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (1 0) (2 0) (3 0))}, :pos (6 8)})
  ;;     :size 10}
  (def updated-game (update-game game (pos/make 0 0) true))
  updated-game
  ;; => {:time 1,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (0 3))}, :pos (6 9)})
  ;;     :size 10}
  (->board updated-game)
  ;; => Execution error (ExceptionInfo) at tetris.graphical/add-block (graphical.clj:67).
  ;;    attempt to write in position outside of the board

  (def game (->game (graphical/from-str-ls
                     ["------------------------------"
                      "                              "
                      "                              "
                      "                              "
                      "                              "
                      "                  [ ][ ][ ][ ]"
                      "                              "
                      "                              "
                      "                              "
                      "                              "
                      "                              "
                      "------------------------------"])))
  game
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (1 0) (2 0) (3 0))}, :pos (6 4)})
  ;;     :size 10}
  (def game-flipped (flip-current-piece game))
  game-flipped
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (0 3))}, :pos (6 4)})
  ;;     :size 10}
  (->board game-flipped)
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "[ ]" "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "[ ]" "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "[ ]" "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "[ ]" "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}

  (def expected-game (->game (graphical/from-str-ls
                              ["------------------------------"
                               "                  [ ]          "
                               "                  [ ]         "
                               "                  [ ]         "
                               "                  [ ]         "
                               "                              "
                               "                              "
                               "                              "
                               "                              "
                               "                              "
                               "                              "
                               "------------------------------"])))
  expected-game
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (0 3))}, :pos (6 0)})
  ;;     :size 10}

  (def piece {:shape {:pos-ls '((0 0) (1 0) (2 0) (3 0))}, :pos '(6 4)})
  (def flipped-piece {:shape {:pos-ls '((0 0) (0 1) (0 2) (0 3))}, :pos '(6 0)})

  (shape/height (piece/shape piece))
;; => 1

  (shape/height (piece/shape flipped-piece))
;; => 4

  (shape/width (piece/shape piece))
;; => 4

  (shape/width (piece/shape flipped-piece))
;; => 1

  (def game-flipped (flip-current-piece game))
  game-flipped
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (0 3))}, :pos (8 0)}),
  ;;     :size 10}
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (0 3))}, :pos (9 1)}),
  ;;     :size 10}
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (0 3))}, :pos (3 7)})
  ;;     :size 10}
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (0 3))}, :pos (3 7)}),
  ;;     :size 10}
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (0 3))}, :pos (3 7)})
  ;;     :size 10}
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (0 3))}, :pos (3 4)})
  ;;     :size 10}
  (->board game-flipped)
  ;; => Execution error (ExceptionInfo) at tetris.graphical/add-block (graphical.clj:67).
  ;;    attempt to write in position outside of the board
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "[ ]" "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "[ ]" "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "[ ]" "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "[ ]" "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}

  ;; the way I flip this piece looks very weird
  (def game
    (->game
     (graphical/from-str-ls ["------------------------------"
                             "   [ ]                        "
                             "   [ ]                        "
                             "   [ ][ ]                     "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "------------------------------"])))

  game
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (1 2))}, :pos (1 0)})
  ;;     :size 10}

  (def flipped-game
    (->game
     (graphical/from-str-ls ["------------------------------"
                             "                              "
                             "         [ ]                  "
                             "   [ ][ ][ ]                  "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "------------------------------"])))

  flipped-game
  ;; => {:time 0,
  ;;     :pieces
  ;;     ({:shape {:pos-ls ((0 1) (1 1) (2 0) (2 1)}, :pos (1 1)}),
  ;;     :size 10}

  ;; this piece in this position was not flipping, even though it looks
  ;; possible
  (def game (->game
             (graphical/from-str-ls ["------------------------------"
                                     "                              "
                                     "            [ ][ ][ ]         "
                                     "               [ ]            "
                                     "                              "
                                     "                              "
                                     "                              "
                                     "                              "
                                     "                              "
                                     "                              "
                                     "                              "
                                     "------------------------------"])))
  game
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (1 0) (1 1) (2 0))}, :pos (4 1)})
  ;;     :size 10}

  (def piece (current-piece game))
  piece
  ;; => {:shape {:pos-ls ((0 0) (1 0) (1 1) (2 0))}, :pos (4 1)}

  (-> piece piece/shape shape/height)
  ;; => 2

  ;; it turned out that I had to adjust the shift in the piece flipping
  (def flipped-piece (piece/flip piece))
  flipped-piece
  ;; => {:shape {:pos-ls ((0 0) (0 1) (1 1) (0 2))}, :pos (5 0)}
  ;; => {:shape {:pos-ls ((0 0) (0 1) (1 1) (0 2))}, :pos (4 -1)}
  (-> flipped-piece piece/shape shape/height)
  ;; => 3

  (let [old-shape (piece/shape piece)
        new-shape (piece/shape flipped-piece)
        old-height (shape/height old-shape)
        old-width (shape/width old-shape)
        new-height (shape/height new-shape)
        new-width (shape/width new-shape)
        old-pos (piece/pos piece)
        width-diff (- old-width new-width)
        height-diff (- old-height new-height)
        shift (pos/make width-diff height-diff)
        new-pos (pos/add old-pos shift)]
    new-pos)

  (defn can-flip?
    [game flipped-piece]
  ;; take the current piece out, flip it and see if it can be moved there
    (can-move? game flipped-piece (piece/pos flipped-piece)))

  (defn flip-current-piece
    "Flips the current piece and updates the game"
    [game]
    (let [piece (current-piece game)
          flipped-piece (piece/flip piece)
          game-without-current (pieces-set! game (rest-pieces game))]
      (if (can-flip? game-without-current flipped-piece)
        (add-piece game-without-current flipped-piece)
        game)))

  (def game-without-current (pieces-set! game (rest (pieces game))))
  (can-flip? game-without-current flipped-piece)
  ;; => true
  ;; => false
  ;; => false
  ;; let's try to figure out the formula for the rotation fromw some examples
  (def game
    (->game
     (graphical/from-str-ls ["------------------------------"
                             "   [ ]                        "
                             "   [ ]                        "
                             "   [ ][ ]                     "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "------------------------------"])))
  game
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (0 2) (1 2))}, :pos (1 0)})
  ;;     :size 10}

  (def flipped-game
    (->game
     (graphical/from-str-ls ["------------------------------"
                             "                              "
                             "   [ ][ ][ ]                  "
                             "   [ ]                        "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "                              "
                             "------------------------------"])))
  flipped-game
  ;; => {:time 0,
  ;;     :pieces ({:shape {:pos-ls ((0 0) (0 1) (1 0) (2 0))}, :pos (1 1)})
  ;;     :size 10}

  (current-piece (flip-current-piece game))
  ;; => {:shape {:pos-ls ((2 0) (1 0) (0 0) (0 1))}, :pos (0 1)}

  (defn flip-test [piece]
    (let [pos-ls (-> piece piece/shape shape/pos-ls)
          new-pos-ls (map (fn [p]
                            (pos/make (* -1 (pos/y p)) (pos/x p))) pos-ls)
          min-x (abs (apply min (cons 0 (map pos/x new-pos-ls))))
          min-y (abs (apply min (cons 0 (map pos/y new-pos-ls))))
          shift (pos/make min-x min-y)
          s-pos-ls (map (partial pos/add shift) new-pos-ls)]
      (piece/make (shape/make s-pos-ls) (piece/pos piece))))

  (flip-test (current-piece game))
  ;; => {:shape {:pos-ls ((2 0) (1 0) (0 0) (0 1))}, :pos (1 0)}
  ;; => {:shape {:pos-ls ((0 0) (-1 0) (-2 0) (-2 1))}, :pos (1 0)}

  (to-str game)
  ;;------------------------------\n
  ;; [ ]                          \n
  ;; [ ]                          \n
  ;; [ ][ ]                       \n
  ;;                              \n
  ;;                              \n
  ;;                              \n
  ;;                              \n
  ;;                              \n
  ;;                              \n
  ;;                              \n
  ;;------------------------------\n
  ;;0                             "

  ;; with this configuration I can still move the piece at the bottom and then a new one is added
  ;; I think I should allow movement left/right and flipping only if the last piece can be moved down
  (def game (->game (graphical/from-str-ls
                     ["------------------------------"
                      "                              "
                      "                              "
                      "                              "
                      "                              "
                      "                              "
                      "                              "
                      "                              "
                      "[ ]                           "
                      "[ ]         [ ][ ]            "
                      "[ ][ ][ ][ ][ ][ ][ ][ ]      "
                      "------------------------------"])))

  ;; testing how to check if a row is full
  (def game (->game (graphical/from-str-ls
                     ["---------------"
                      "               "
                      "               "
                      "               "
                      "[ ][ ][ ]   [ ]"
                      "[ ][ ][ ][ ][ ]"
                      "---------------"])))

  game
  ;; => {:time 0,
  ;;     :pieces
  ;;     ({:shape {:pos-ls ((0 0))}, :pos (4 3)}
  ;;      {:shape
  ;;       {:pos-ls ((0 0) (0 1) (1 0) (1 1) (2 0) (2 1) (3 1) (4 1))},
  ;;       :pos (0 3)}),
  ;;     :size 5,
  ;;     :points 0}

  (def pieces-ls (pieces game))
  pieces-ls
  ;; => ({:shape {:pos-ls ((0 0))}, :pos (4 3)}
  ;;     {:shape {:pos-ls ((0 0) (0 1) (1 0) (1 1) (2 0) (2 1) (3 1) (4 1))},
  ;;      :pos (0 3)})

  (def all-pos-ls (mapcat piece/->pos-ls pieces-ls))
  all-pos-ls
  ;; => ((4 3) (0 3) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4) (4 4))

  ;; check row 5
  (def pos-ls-in-row-5 (filter (fn [p] (= (pos/y p) 4)) all-pos-ls))
  pos-ls-in-row-5
  ;; => ((0 4) (1 4) (2 4) (3 4) (4 4))
  (count pos-ls-in-row-5)

  ;; combining everything
  ;; => 5
  (let [pieces-ls (pieces game)
        all-pos-ls (mapcat piece/->pos-ls pieces-ls)
        pos-ls-in-row-5 (filter (fn [p] (= (pos/y p) 4)) all-pos-ls)]
    (= (count pos-ls-in-row-5) 5))
  ;; => true

  (let [pieces-ls (pieces game)
        all-pos-ls (mapcat piece/->pos-ls pieces-ls)
        pos-ls-in-row-4 (filter (fn [p] (= (pos/y p) 3)) all-pos-ls)]
    (= (count pos-ls-in-row-4) 5))
  ;; => false
  (row-is-full? game 4)
  ;; => true
  (row-is-full? game 3)
  ;; => false

  (def full-rows-ls (full-rows game))
  full-rows-ls
  ;; => (4)

  ((comp not empty?) '(1 2 3))
  ;; => true

  ((comp not empty?) '())
  ;; => false

  (def game-without-full-rows (remove-full-rows game full-rows-ls))
  game-without-full-rows
  ;; => {:time 0,
  ;;     :pieces
  ;;     ({:shape {:pos-ls ((0 0))}, :pos (4 3)}
  ;;      {:shape {:pos-ls ((0 1) (1 1) (2 1))}, :pos (0 3)}),
  ;;     :size 5,
  ;;     :points 0}

  (to-str game-without-full-rows)
  ;; "---------------"
  ;; "               "
  ;; "               "
  ;; "               "
  ;; "            [ ]"
  ;; "[ ][ ][ ]      "
  ;; "---------------"

  (def shape {:pos-ls '((0 0) (0 1) (1 0) (1 1) (2 0) (2 1) (3 1) (4 1))})
  (def row 1)
  (vals (group-by (fn [p] (= (pos/y p) row)) (shape/pos-ls shape)))
  ;; => ([(0 0) (1 0) (2 0)] [(0 1) (1 1) (2 1) (3 1) (4 1)])

  (def other-pos ['(0 0) '(1 0) '(2 0)])

  (def pos-above-row? (fn [p] (< (pos/y p) row)))
  ;; I was using group-by to split the elements in a list that satisfied a predicate and the others
  ;; but when everything satistifies the predicate then we get only one list when doing vals
  (group-by pos-above-row? other-pos)
  ;; => {true [(0 0) (1 0) (2 0)]}
  ((group-by pos-above-row? other-pos) false)
  ;; => nil
  (vals (group-by pos-above-row? other-pos))
  ;; => ([(0 0) (1 0) (2 0)])

  ;; let's apply gravity and make all the position that can go down, go down
  (def down-by-1 (pos/make 0 1))
  (def can-move-piece?
    (fn [piece]
      (can-move? game-without-full-rows piece movement/move-down)))

  (def piece-that-can-be-moved (first (pieces game-without-full-rows)))
  piece-that-can-be-moved
  ;; => {:shape {:pos-ls ((0 0))}, :pos (4 3)}

  (can-move-piece? piece-that-can-be-moved)
  ;; => true

  (def pieces-that-can-be-moved
    (filter can-move-piece?
            (pieces game-without-full-rows)))
  pieces-that-can-be-moved
  ;; => ({:shape {:pos-ls ((0 0))}, :pos (4 3)})

  (def game-with-moved-pieces
    (reduce (fn [game piece]
              (let [new-pos (pos/add (piece/pos piece) down-by-1)]
                (move-piece game piece new-pos)))
            game-without-full-rows
            pieces-that-can-be-moved))
  game-with-moved-pieces
  ;; => {:time 0,
  ;;     :pieces
  ;;     ({:shape {:pos-ls ((0 0))}, :pos (4 4)}
  ;;      {:shape {:pos-ls ((0 1) (1 1) (2 1))}, :pos (0 3)}),
  ;;     :size 5,
  ;;     :points 0}
  (to-str game-with-moved-pieces)
  ;;---------------\n
  ;;               \n
  ;;               \n
  ;;               \n
  ;;               \n
  ;;[ ][ ][ ]   [ ]\n
  ;;---------------

  ;; we will need to keep doing this until there are no pieces to move

;; separator :P
  )
