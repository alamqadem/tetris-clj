(ns tetris.game
  (:require [tetris.graphical :as graphical]
            [tetris.position :as pos]
            [tetris.shape :as shape]
            [tetris.piece :as piece]))
;; game
(defn make-game [time pieces size]
  {:time time,
   :pieces pieces,
   :size size})

(defn game-time [game]
  (get game :time))

(defn game-pieces [game]
  (get game :pieces))

(defn game-size [game]
  (get game :size))

(defn game-time-set! [game time]
  (make-game
   time
   (game-pieces game)
   (game-size game)))

(defn game-pieces-set! [game pieces]
  (make-game (game-time game) pieces (game-size game)))

(defn game-size-set! [game size]
  (make-game (game-time game) (game-pieces game) size))

(defn current-piece [game]
  ;; given a game returns the current piece that is failling
  (last (game-pieces game)))

(defn add-piece [game piece]
  ;; adds a piece in such a way that the new piece is the current piece
  (game-pieces-set! game
                    (conj (game-pieces game) piece)))

(defn game->board [game]
  ;; transforms a game into a board
  (let [pieces (game-pieces game)
        board (graphical/create-board (game-size game))]
    (reduce (fn [board p] (piece/piece->board p board)) board pieces)))

(defn pos-in-game? [game pos]
  ;; returns true if a position is within a game matrix
  (and (< (pos/pos-x pos) (game-size game))
       (< (pos/pos-y pos) (game-size game))))

(defn outside-of-boundaries? [game piece pos]
  ;; returns true if moving piece in the game to pos makes it outside of the game boundaries
  (let [pos-ls (piece/piece->pos-ls (piece/piece-pos-set! piece pos))]
    (some not (map (fn [p] (pos-in-game? game p)) pos-ls))))

(defn collision-with-other-piece? [game piece pos]
  ;; returns true if piece moved to pos collides with another piece
  (let [other-pieces (filter (fn [p] (not= p piece)) (game-pieces game))
        other-pieces-pos-ls (mapcat piece/piece->pos-ls other-pieces)
        piece-pos-ls-after-move (piece/piece->pos-ls (piece/piece-pos-set! piece pos))]
    (pos/pos-ls-intersect? other-pieces-pos-ls piece-pos-ls-after-move)))

(defn move-piece? [game piece pos]
  ;; true if it can move a piece to pos, false otherwise
  (not (or
        (outside-of-boundaries? game piece pos)
        (collision-with-other-piece? game piece pos))))

(defn move-piece [game piece new-pos]
  (let [new-piece (piece/piece-pos-set! piece new-pos)
        pieces (into [] (filter (fn [p] (not= p piece)) (game-pieces game)))]
    (game-pieces-set! game (conj pieces new-piece)))
  )

(defn move-piece-down [game piece]
  (let [new-pos (pos/pos-add (piece/piece-pos piece) (pos/make-pos 0 1))]
    (move-piece game piece new-pos)))

(defn add-random-piece [game]
  ;;adds a new piece of a random shape at the top of the game in a random position
  (let [rand-shape (shape/random-shape!)
        rand-shape-width (shape/shape-width rand-shape)
        rand-pos (pos/rand-pos (- (game-size game) rand-shape-width))
        rand-piece (piece/make-piece rand-shape rand-pos)]
    (add-piece game rand-piece)))

(defn game-over? [game]
  (let [piece (current-piece game)
        pos (piece/piece-pos piece)
        new-pos (pos/pos-add pos (pos/make-pos 0 1))]
    (and
     (= (pos/pos-y pos) 0)
     (not (move-piece? game piece new-pos)))))
  
(defn update-game [game]
  (let [game-updated (game-time-set! game (inc (game-time game)))
        piece (current-piece game-updated)
        down-by-1 (pos/pos-add (piece/piece-pos piece) (pos/make-pos 0 1))]
    ;; increment time
    ;; move current piece down by 1
    ;; if the current piece cannot move add a new random piece
    ;; move pieces down by 1
    (if (move-piece? game-updated piece down-by-1)
      (move-piece game-updated piece down-by-1)
      (add-random-piece game-updated))))

  (comment
  {:time 0,
   :pieces [nil, nil, nil],
   :size 5}

  (make-game 0 [] 10)
  ;; => {:time 0, :pieces [], :size 10}

  (pos-in-game? (make-game 0 [] 10) (pos/make-pos 1 1))
  ;; => true

  (outside-of-boundaries? (make-game 0 [] 10) (piece/make-piece shape/l-shape (pos/make-pos 0 0)) (pos/make-pos 1 1))
  ;; => false
  (outside-of-boundaries? (make-game 0 [] 10) (piece/make-piece shape/l-shape (pos/make-pos 0 0)) (pos/make-pos 1 10))
  ;; => true
  (pos/pos-in-pos-ls? (pos/make-pos 0 1) [(pos/make-pos 1 1) (pos/make-pos 2 1) (pos/make-pos 0 1)])
  ;; => true

  (pos/pos-in-pos-ls? (pos/make-pos 0 1) [(pos/make-pos 1 1) (pos/make-pos 2 1) (pos/make-pos 0 0)])
  ;; => false

  (pos/pos-ls-intersect? [(pos/make-pos 0 0) (pos/make-pos 1 1)] [(pos/make-pos 1 0) (pos/make-pos 2 1)])
  ;; => false

  (pos/pos-ls-intersect? [(pos/make-pos 0 0) (pos/make-pos 1 1)] [(pos/make-pos 1 0) (pos/make-pos 1 1)])
  ;; => true
  (def game
    (make-game 0 [(piece/make-piece shape/l-shape (pos/make-pos 1 1))
                  (piece/make-piece shape/block-shape (pos/make-pos 3 0))]
               10))

  (def piece (get (game-pieces game) 1))
  (def pos (pos/make-pos 1 1))
  ;; (fold

  (def board (graphical/create-board 5))

  (let [piece (piece/make-piece shape/block-shape (pos/make-pos 3 0))
        other-pieces (filter (fn [p] (not= p piece)) (game-pieces game))
        other-pieces-pos-ls (map piece/piece->pos-ls other-pieces)]
    other-pieces-pos-ls)
  ;; => (((1 1) (1 2) (2 1) (3 1)) ((3 1)))

  (loop [board1 board,
         pieces [(piece/make-piece shape/l-shape (pos/make-pos 0 0))
                 (piece/make-piece shape/block-shape (pos/make-pos 4 0))
                 (piece/make-piece shape/block-shape (pos/make-pos 0 3))]]
    (if (empty? pieces)
      board1
      (recur (piece/piece->board (first pieces) board1)
             (rest pieces))))
  ;; => {:board
  ;;     [["[ ]" "[ ]" "[ ]" "   " "[ ]"]
  ;;      ["[ ]" "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]],
  ;;     :size 5}
  (let [pieces (game-pieces game)]
    (reduce (fn [board p] (piece/piece->board p board)) board pieces))
  ;; => {:board
  ;;     [["   " "   " "   " "[ ]" "   "]
  ;;      ["   " "[ ]" "[ ]" "[ ]" "   "]
  ;;      ["   " "[ ]" "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]],
  ;;     :size 5}
  (current-piece game)
  ;; => {:shape {:pos-ls [(0 0) (0 1) (1 0) (2 0)]}, :pos (1 1)}
  (add-piece game (piece/make-piece shape/square-shape (pos/make-pos 0 3)))
  ;; => {:time 0,
  ;;     :pieces
  ;;     [{:shape {:pos-ls [(0 0) (0 1) (1 0) (2 0)]}, :pos (1 1)}
  ;;      {:shape {:pos-ls [(0 0)]}, :pos (3 0)}
  ;;      {:shape {:pos-ls [(0 0) (0 1) (1 0) (1 1)]}, :pos (0 3)}],
  ;;     :size 10}

  (def empty-game
    (make-game 0 [] 10))

  (game->board empty-game)
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

  (game->board (add-random-piece empty-game))
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
    (make-game 0 [(piece/make-piece shape/l-shape (pos/make-pos 0 7))] 10))

  (game->board game-with-bug)
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
  (game->board game-with-bug1)
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
  (game->board game-with-bug2)
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
  (game->board game-with-bug3)
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
  (game-pieces game-with-bug2)
  ;; => ({:shape {:pos-ls [(0 0) (1 0) (2 0) (1 1)]}, :pos (2 1)}
  ;;     {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)})
  ;; => ({:shape {:pos-ls [(1 0) (1 1) (1 2) (0 2)]}, :pos (1 1)}
  ;;     {:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)})

  (game-pieces game-with-bug1)
  ;; => [{:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)}
  ;;     {:shape {:pos-ls [(0 0) (1 0) (2 0) (1 1)]}, :pos (2 0)}]
  ;; => [{:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 7)}
  ;;     {:shape {:pos-ls [(1 0) (1 1) (1 2) (0 2)]}, :pos (1 0)}]

  (def game-test-collision
    (add-piece
     (make-game 0 [(piece/make-piece shape/l-shape (pos/make-pos 0 2))] 5)
     (piece/make-piece shape/square-shape (pos/make-pos 0 0))))

  (game->board game-test-collision)
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
    (pos/pos-add (piece/piece-pos sq-piece) (pos/make-pos 0 1)))

  new-pos
  ;; => (0 1)

  (collision-with-other-piece? game-test-collision sq-piece new-pos)
  ;; => nil

  (move-piece? game-test-collision sq-piece new-pos)

  (def updated-game (update-game game-test-collision))

  (game->board updated-game)
  ;; => {:board
  ;;     [["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "   " "   " "   "]],
  ;;     :size 5}

  (def other-pieces (filter (fn [p] (not= p sq-piece)) (game-pieces game-test-collision)))
  other-pieces
  ;; => ({:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (0 2)})
  (def other-pieces-pos-ls (map piece/piece->pos-ls other-pieces))
  other-pieces-pos-ls
  ;; => (((0 2) (0 3) (0 4) (1 4)))
  (mapcat piece/piece->pos-ls other-pieces)
  (def piece-pos-ls (piece/piece->pos-ls sq-piece))
  piece-pos-ls
  ;; => ((0 0) (0 1) (1 0) (1 1))
  (def piece-pos-ls-after-move (map (fn [p] (pos/pos-add p new-pos)) piece-pos-ls))
  piece-pos-ls-after-move
  ;; => ((0 1) (0 2) (1 1) (1 2))
  (pos/pos-ls-intersect? other-pieces-pos-ls piece-pos-ls-after-move)
  ;; => nil
  (pos/pos-ls-intersect? (reduce concat other-pieces-pos-ls) piece-pos-ls-after-move)
  ;; => true
  (def game-test-collision1
    (make-game 0 [
                  (piece/make-piece shape/l-shape (pos/make-pos 7 7))
                  (piece/make-piece shape/t-shape (pos/make-pos 6 5))] 10))
  (graphical/print-board (game->board game-test-collision1))
  (def game-test-collision1-updated
    (update-game game-test-collision1))
  (graphical/print-board (game->board game-test-collision1-updated))
  (def curr-piece (current-piece game-test-collision1))
  curr-piece
  ;; => {:shape {:pos-ls [(0 0) (1 0) (2 0) (1 1)]}, :pos (6 5)}
  (def new-pos (pos/pos-add (pos/make-pos 6 5) (pos/make-pos 0 1)))
  new-pos
  ;; => (6 6)
  (move-piece? game-test-collision1 curr-piece new-pos)
  (collision-with-other-piece? game-test-collision1 curr-piece new-pos)
  (def other-pieces (filter (fn [p] (not= p curr-piece)) (game-pieces game-test-collision1)))
  other-pieces
  ;; => ({:shape {:pos-ls [(0 0) (0 1) (0 2) (1 2)]}, :pos (7 7)})
  (def other-pieces-pos-ls (mapcat piece/piece->pos-ls other-pieces))
  other-pieces-pos-ls
  ;; => ((7 7) (7 8) (7 9) (8 9))
  (def piece-pos-ls (piece/piece->pos-ls curr-piece))
  piece-pos-ls
  ;; => ((6 5) (7 5) (8 5) (7 6))
  (def piece-pos-ls-after-move (map (fn [p] (pos/pos-add p new-pos)) piece-pos-ls))
  piece-pos-ls-after-move
  ;; => ((12 11) (13 11) (14 11) (13 12))
  (pos/pos-ls-intersect? other-pieces-pos-ls piece-pos-ls-after-move)
  ;; => nil

  (def piece-pos-ls-after-move (piece/piece->pos-ls (piece/piece-pos-set! curr-piece new-pos)))
  piece-pos-ls-after-move
  ;; => ((6 6) (7 6) (8 6) (7 7))
  (pos/pos-ls-intersect? other-pieces-pos-ls piece-pos-ls-after-move)
  ;; => true

  )
