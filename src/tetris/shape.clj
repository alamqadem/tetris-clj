(ns tetris.shape
  (:require [tetris.position :as pos]))

;; shapes
(defn make [pos-ls] {:pos-ls pos-ls})

(defn pos-ls [shape] (get shape :pos-ls))

(defn width [shape]
  ;; returns the width of a shape
  ;; the real implemenation should be (- (max pos/x) (min pos/x))
  (inc
   (reduce max
           (map pos/x (pos-ls shape)))))

(defn height [shape]
  ;; returns the height of a shape
  (inc
   (reduce max
           (map pos/y (pos-ls shape)))))

(defn normalize
  "Returns a shape where all the positions in pos-ls have pos subtracted"
  [shape pos]
  (make
   (->>
    (pos-ls shape)
    (map (fn [p] (pos/sub p pos)))
    (sort pos/compare-pos))))

(def block (make [(pos/make 0 0)]))

(def l (make [(pos/make 0 0)
              (pos/make 0 1)
              (pos/make 0 2) (pos/make 1 2)]))

(def square
  (make [(pos/make 0 0) (pos/make 0 1)
         (pos/make 1 0) (pos/make 1 1)]))

(def i
  (make [(pos/make 0 0)
         (pos/make 0 1)
         (pos/make 0 2)
         (pos/make 0 3)]))

(def s
  (make [               (pos/make 1 0) (pos/make 2 0)
         (pos/make 0 1) (pos/make 1 1)]))

(def z
  (make [(pos/make 0 0) (pos/make 1 0)
                        (pos/make 1 1) (pos/make 2 1)]))

(def j
  (make [               (pos/make 1 0)
                        (pos/make 1 1)
         (pos/make 0 2) (pos/make 1 2)]))

(def t
  (make [(pos/make 0 0) (pos/make 1 0) (pos/make 2 0)
                          (pos/make 1 1)]))
(def shapes
  [l square i s z j t])

(defn random-shape []
  (get shapes (rand-int (count shapes))))

(comment
  (count shapes)
  (rand-int (count shapes))
  (get shapes (rand-int (count shapes)))
  (pos-ls l)
  ;; => [(0 0) (0 1) (0 2) (1 2)]
  ;; calculate width
  (reduce max (map pos/x (pos-ls l)))l
  ;; => 1
  )

