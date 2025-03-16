(ns tetris.position-test
  (:require [clojure.test :refer :all]
            [tetris.position :refer :all]))

(deftest test-pos-operations
  (testing "adding 2 positions"
    (is (= (add (make 1 1) (make 2 1)) (make 3 2)))
    (is (= (add (make 0 0) (make 0 0)) (make 0 0))))
  (testing "positions equality"
    (is (eq? (make 0 0) (make 0 0)))
    (is (not (eq? (make 0 0) (make 1 0))))))

(deftest test-pos-list-operations
  (testing "check if a position is in a list"
    (is (in-pos-ls? (make 0 0) [(make 1 1) (make 2 1) (make 0 0)]))
    (is (not (in-pos-ls? (make 0 0) [(make 1 1) (make 2 1)]))))
  (testing "if 2 list of positions intersect"
    (is (pos-ls-intersect?
         [(make 1 1) (make 2 1) (make 0 0)]
         [(make 3 1) (make 2 0) (make 0 0)]))
    (is (not
         (pos-ls-intersect?
          [(make 1 1) (make 2 1) (make 0 0)]
          [(make 3 1) (make 2 0) (make 0 1)])))
    (is (pos-ls-intersect?
         [(make 0 2) (make 0 3) (make 0 4) (make 1 4)]
         [(make 0 1) (make 0 2) (make 1 1) (make 1 2)]))))


