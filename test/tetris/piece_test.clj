(ns tetris.piece-test
  (:require [clojure.test :refer :all]
            [tetris.piece :refer :all]
            [tetris.shape :as shape]
            [tetris.position :as pos]
            [tetris.graphical :as graphical]))

(deftest test-piece-operations
  (testing "piece to list of positions"
    (let [simple-l (make shape/l (pos/make 0 0))]
      (is (= (->pos-ls simple-l) (shape/pos-ls shape/l))))
    (let [l-in-1x1 (make shape/l (pos/make 1 1))]
      (is (= (->pos-ls l-in-1x1) [(pos/make 1 1)
                                  (pos/make 1 2)
                                  (pos/make 1 3) (pos/make 2 3)]))))
  (testing "piece to a board"
    (let [simple-l (make shape/l (pos/make 0 0))
          empty-board (graphical/make 5)
          add-block (fn [x y board] (graphical/add-block board x y))
          expected-board (->>
                          empty-board
                          (add-block 0 0)
                          (add-block 0 1)
                          (add-block 0 2)
                          (add-block 1 2))]
      (is (= (->board simple-l empty-board) expected-board)))
    (let [l-in-1x1 (make shape/l (pos/make 1 1))
          empty-board (graphical/make 5)
          add-block (fn [x y board] (graphical/add-block board x y))
          expected-board (->>
                          empty-board
                          (add-block 1 1)
                          (add-block 1 2)
                          (add-block 1 3)
                          (add-block 2 3))]
      (is (= (->board l-in-1x1 empty-board) expected-board)))))
