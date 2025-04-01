(ns tetris.movement-test
  (:require [clojure.test :refer :all]
            [tetris.movement :refer :all]
            [tetris.position :as pos]
            [tetris.piece :as piece]
            [tetris.shape :as shape]))

(deftest test-movements
  (let [piece (piece/make shape/l (pos/make 0 0))]
    (testing "singular movements"
      (is (piece/eq?
           (move piece move-right)
           {:shape {:pos-ls [(pos/make 0 0) (pos/make 0 1) (pos/make 0 2) (pos/make 1 2)]},
            :pos (pos/make 1 0)}))
      (is (piece/eq?
           (move piece move-left)
           {:shape {:pos-ls [(pos/make 0 0) (pos/make 0 1) (pos/make 0 2) (pos/make 1 2)]},
            :pos (pos/make -1 0)}))
      (is (piece/eq?
           (move piece move-down)
           {:shape {:pos-ls [(pos/make 0 0) (pos/make 0 1) (pos/make 0 2) (pos/make 1 2)]},
            :pos (pos/make 0 1)}))
      (is (piece/eq?
           (move piece no-movement)
           {:shape {:pos-ls [(pos/make 0 0) (pos/make 0 1) (pos/make 0 2) (pos/make 1 2)]},
            :pos (pos/make 0 0)}))
      (is (piece/eq?
           (move piece flip-move)
           {:shape {:pos-ls (list (pos/make 2 0) (pos/make 1 0) (pos/make 0 0) (pos/make 0 1))},
            :pos (pos/make -1 1)}))))

  (testing "combining movements"
    (let [piece (piece/make shape/l (pos/make 0 0))]
      (is (piece/eq?
           (move piece (combine  move-right move-left))
           {:shape {:pos-ls [(pos/make 0 0) (pos/make 0 1) (pos/make 0 2) (pos/make 1 2)]},
            :pos (pos/make 0 0)}))
      (is (piece/eq?
           (move piece (combine move-right move-left move-down))
           {:shape {:pos-ls [(pos/make 0 0) (pos/make 0 1) (pos/make 0 2) (pos/make 1 2)]},
            :pos (pos/make 0 1)}))
      (is (piece/eq?
           (move piece (combine move-right))
           {:shape {:pos-ls [(pos/make 0 0) (pos/make 0 1) (pos/make 0 2) (pos/make 1 2)]},
            :pos (pos/make 1 0)}))
      (is (piece/eq?
           (move piece (combine))
           {:shape {:pos-ls [(pos/make 0 0) (pos/make 0 1) (pos/make 0 2) (pos/make 1 2)]},
            :pos (pos/make 0 0)}))
      (is (piece/eq?
           (move piece (combine flip-move move-down))
           {:shape {:pos-ls (list (pos/make 2 0) (pos/make 1 0) (pos/make 0 0) (pos/make 0 1))},
            :pos (pos/make -1 2)})))))
