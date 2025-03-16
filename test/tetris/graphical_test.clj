(ns tetris.graphical-test
  (:require [clojure.test :refer :all]
            [tetris.graphical :refer :all]))

(deftest matrix-create
  (testing "that row returns a vector"
    (is (= (row 1) ["   "]))
    (is (vector? (row 1))))
  (testing "that matrix returns a vector"
    (is (= (make-matrix 2) [["   " "   "]
                            ["   " "   "]]))
    (is (vector? (make-matrix 2)))))

(deftest board-operations
  (let [board (make 5)]
    (testing "check if a pos is in the board"
      (is (pos-in-board? 0 0 board))
      (is (pos-in-board? 2 0 board))
      (is (not (pos-in-board? 5 0 board)))
      (is (not (pos-in-board? 0 5 board))))))

(deftest test-add-block
  (let [board (make 5)]
    (testing "adding a block inside the board"
      (is (=
           (add-block board 1 1)
           {:board [["   ", "   ", "   ", "   ", "   "]
                    ["   ", "[ ]", "   ", "   ", "   "]
                    ["   ", "   ", "   ", "   ", "   "]
                    ["   ", "   ", "   ", "   ", "   "]
                    ["   ", "   ", "   ", "   ", "   "]]
            :size 5})))
    (testing "adding a block outside of the board"
      (is (thrown? RuntimeException (add-block board 0 5))))))

(deftest test-print-board
  (let [board (make 5)]
    (testing "to string works correctly"
      (is (=
           (to-str board)
           (str "               \n"
                "               \n"
                "               \n"
                "               \n"
                "               ")))
      (is (=
           (to-str (add-block board 4 4))
           (str "               \n"
                "               \n"
                "               \n"
                "               \n"
                "            [ ]"))))))




