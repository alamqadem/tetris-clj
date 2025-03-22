(ns tetris.game-test
  (:require [clojure.test :refer :all]
            [tetris.game :refer :all]
            [tetris.position :as pos]
            [tetris.shape :as shape]
            [tetris.piece :as piece]
            [tetris.graphical :as graphical]))

(deftest test-game-operations
  (testing "adding the a piece and then getting returns the same piece"
    (let [block-piece (piece/make shape/block (pos/make 3 0))
          game (make 0 [block-piece] 10)
          l-piece (piece/make shape/l (pos/make 1 1))
          game-with-new-piece (add-piece game l-piece)]
      (is (= (current-piece game-with-new-piece) l-piece))))
  (testing "check if a row is full"
    (let [game (->game
                (graphical/from-str-ls
                 ["---------------"
                  "               "
                  "               "
                  "               "
                  "[ ][ ][ ]   [ ]"
                  "[ ][ ][ ][ ][ ]"
                  "---------------"]))]
      (is (row-is-full? game 4))
      (is (not (row-is-full? game 3))))))

(deftest test-game-collision
  (testing "piece outside of game boundaries"
    (let [game (make 0 [(piece/make shape/l (pos/make 0 2))] 5)
          ;; => {:board
          ;;     [["   " "   " "   " "   " "   "]
          ;;      ["   " "   " "   " "   " "   "]
          ;;      ["[ ]" "   " "   " "   " "   "]
          ;;      ["[ ]" "   " "   " "   " "   "]
          ;;      ["[ ]" "[ ]" "   " "   " "   "]],
          ;;     :size 5}
          current-piece (current-piece game)
          new-pos (pos/make 0 3)]
      (is (outside-of-boundaries? game current-piece new-pos))
      (is (not (outside-of-boundaries? game current-piece (piece/pos current-piece))))
      (is (outside-of-boundaries? game current-piece (pos/make -1 1)))))


  (testing "collision detection of pieces"
    (let [game-test-collision
          (add-piece
           (make 0 [(piece/make shape/l (pos/make 0 2))] 5)
           (piece/make shape/square (pos/make 0 0)))
          ;; => {:board
          ;;     [["[ ]" "[ ]" "   " "   " "   "]
          ;;      ["[ ]" "[ ]" "   " "   " "   "]
          ;;      ["[ ]" "   " "   " "   " "   "]
          ;;      ["[ ]" "   " "   " "   " "   "]
          ;;      ["[ ]" "[ ]" "   " "   " "   "]],
          ;;     :size 5}
          sq-piece (current-piece game-test-collision)
          new-pos (pos/add (piece/pos sq-piece) (pos/make 0 1))]
      (is (collision-with-other-piece? game-test-collision sq-piece new-pos))
      (is (not (collision-with-other-piece? game-test-collision sq-piece (piece/pos sq-piece)))))))

