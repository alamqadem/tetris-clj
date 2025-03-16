(ns tetris.shape-test
    (:require [clojure.test :refer :all]
              [tetris.shape :refer :all]))

(deftest test-shape-operations
  (testing "width and lenght"
    (is (= (width block) 1))
    (is (= (height block) 1))
    (is (= (width l) 2))
    (is (= (height l) 3))
    (is (= (width s) 3))
    (is (= (height s) 2))
    (is (= (width j) 2))
    (is (= (height j) 3))
    (is (= (width t) 3))))
