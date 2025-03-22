(ns tetris.position-test
  (:require [clojure.test :refer :all]
            [tetris.position :refer :all]))

(deftest test-pos-operations
  (testing "adding 2 positions"
    (is (= (add (make 1 1) (make 2 1)) (make 3 2)))
    (is (= (add (make 0 0) (make 0 0)) (make 0 0))))
  (testing "positions equality"
    (is (eq? (make 0 0) (make 0 0)))
    (is (not (eq? (make 0 0) (make 1 0)))))
  (testing "position flipping"
    (is (eq? (flip-90 (make 0 1)) (make -1 0))))
  (testing "position abs operation"
    (is (eq? (abs-pos (make -1 0)) (make 1 0)))
    (is (eq? (abs-pos (make 1 0)) (make 1 0)))
    (is (eq? (abs-pos (make 0 0)) (make 0 0)))
    (is (eq? (abs-pos (make 1 -1)) (make 1 1)))
    )
  (testing "position minimum"
    (is (eq? (min-pos (list (make 1 2) (make 3 0))) (make 1 0))))
  (testing "contiguos operations"
    (let [pos-ls (list (make 1 0) (make 2 0) (make 2 1) (make 3 0)
                       (make 3 4) (make 4 2) (make 4 3) (make 4 4))]
      (is (contiguos? (make 1 0) (make 2 0)))
      (is (not (contiguos? (make 1 0) (make 1 3))))
      (is (contiguos-to-ls? (make 1 0)
                            (list (make 2 0) (make 2 1) (make 3 0)
                                  (make 3 4) (make 4 2) (make 4 3) (make 4 4))))
      (is (= (find-contiguous-group pos-ls)
             (list
              (list (make 3 0) (make 2 1) (make 2 0) (make 1 0))
              (list (make 4 4) (make 4 3) (make 4 2) (make 3 4)))))
      (is (= (find-contiguous pos-ls)
             (list
              (list (make 3 4) (make 4 2) (make 4 3) (make 4 4))
              (list (make 3 0) (make 2 1) (make 2 0) (make 1 0)))))))
  (testing "sorting"
    (is (= (sort compare-pos (list (make 0 2) (make 1 0) (make 1 1) (make 1 2)))
           (list (make 0 2) (make 1 0) (make 1 1) (make 1 2))))))

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


