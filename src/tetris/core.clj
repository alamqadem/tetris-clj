(ns tetris.core
  (:gen-class)
  (:require [tetris.graphical :as graphical]
            [tetris.position :as pos]
            [tetris.shape :as shape]
            [tetris.piece :as piece]
            [tetris.movement :as movement]
            [tetris.game :as game]
            ))

;; loop
(defn game-loop [board-size]
  (let [empty-game (game/make 0 [] board-size)]
    (loop [game empty-game]
      (game/print-game game)
      (let [input (read-line)]
        (if (not= input "q")
          (let [movement (case input
                           "a" movement/move-left
                           "d" movement/move-right
                           "s" movement/flip-move
                           movement/no-movement)]

            (flush)
            (if (game/game-over? game)
              (println "game over...")
              (recur (game/update-game game movement))))
          (println "quitting..."))))))

(defn -main
  [& args]
  (let [size (if (empty? args)
               20
               (Integer/parseInt (first args)))]
    (game-loop size)))

(comment
  (game-loop 5)

  {:pieces [{:piece
             {:kind :block, :pos ['(3 2), '(1 1)], :pos-size 2},
             :pos '(3 3)}]
   :size 5}
  ;; pos
  ;; make-pos
  (def pos ((fn [x y] (list x y)) 3 0))
  ;; pos-x
  ((fn [pos] (first pos)) pos)
  ;; pos-y
  ((fn [pos] (last pos)) pos)

  ;; shape
  ;; make-shape
  (def shape
    ((fn [pos-ls] {:pos-ls pos-ls}) [(list 0 0)]))

  ;; shape-pos-ls
  ((fn [shape] (get shape :pos-ls)) shape)

  ;; testing that drawing a shape works as expected in graphical
  ;; draw-shape
  (def board-with-a-shape
    ((fn [shape board]
       (let [pos-ls (get shape :pos-ls)]
         (loop [current-board board, current-pos-ls pos-ls]
           (if (empty? current-pos-ls)
             current-board
             (let [current-pos (first current-pos-ls),
                   new-board (graphical/add-block current-board (first current-pos) (last current-pos))]
               (recur new-board (rest current-pos-ls)))))))
     {:pos-ls [(list 0 0) (list 0 1) (list 1 0) (list 2 0)]} (graphical/make 20)))
  ;; => {:board
  ;;     [["[ ]" "[ ]" "[ ]" "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]],
  (graphical/print-board board-with-a-shape)

  (def board-with-a-piece (piece/->board (piece/make shape/l (pos/make 1 2)) (graphical/make 5)))
  board-with-a-piece
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   "]
  ;;      ["   " "[ ]" "   " "   " "   "]
  ;;      ["   " "[ ]" "   " "   " "   "]
  ;;      ["   " "[ ]" "[ ]" "   " "   "]],
  ;;     :size 5}
  (graphical/print-board board-with-a-piece)

;; trying to reproduce a bug
  ;;   ------------------------------
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;; [ ][ ][ ]                     
  ;; [ ]                           
  ;; ------------------------------
  ;; ------------------------------
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;;                              
  ;; [ ][ ][ ]                     
  ;; [0 "[ ]"]
  ;; ------------------------------
  (def board-size 10)
  (def game (game/make 0 [(piece/make shape/l (pos/make 0 8))] board-size))

  (game/->board game)
  ;; => {:board
  ;;     [["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["   " "   " "   " "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "[ ]" "[ ]" "   " "   " "   " "   " "   " "   " "   "]
  ;;      ["[ ]" "   " "   " "   " "   " "   " "   " "   " "   " "   "]],
  ;;     :size 10}

  (def piece (first (game/pieces game)))
  (def new-pos (pos/add (piece/pos piece) (pos/make 0 1)))

  (game/can-move? game piece movement/move-down)
  ;; => true
  (game/outside-of-boundaries? game piece new-pos)
  ;; => false
  (let [pos-ls
        (piece/->pos-ls (piece/pos-set! piece new-pos))
          ;; => ((0 9) (0 10) (1 9) (2 9))
        ]
    (some not (map (fn [p] (pos/within-boundaries? p (pos/make (game/size game) (game/size game)))) pos-ls)))

  (def invalid-game
    (game/pieces-set! game
                      (piece/pos-set! piece new-pos)))

  (game/->board invalid-game)
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

  ;; separator :P
  )
  
  
