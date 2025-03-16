(ns tetris.shape
  (:require [tetris.position :as pos]))

;; shapes
(defn make-shape [pos-ls] {:pos-ls pos-ls})

(defn shape-pos-ls [shape] (get shape :pos-ls))

(defn shape-width [shape]
  ;; returns the width of a shape
  (reduce max (map pos/pos-x (shape-pos-ls shape))))

(defn shape-height [shape]
  ;; returns the height of a shape
  (reduce max (map pos/pos-y (shape-pos-ls shape))))

(def block-shape (make-shape [(list 0 0)]))

(def l-shape (make-shape [(pos/make-pos 0 0)
                          (pos/make-pos 0 1)
                          (pos/make-pos 0 2) (pos/make-pos 1 2)]))

(def square-shape
  (make-shape [(pos/make-pos 0 0) (pos/make-pos 0 1)
               (pos/make-pos 1 0) (pos/make-pos 1 1)]))

(def i-shape
  (make-shape [(pos/make-pos 0 0)
               (pos/make-pos 0 1)
               (pos/make-pos 0 2)
               (pos/make-pos 0 3)]))

(def s-shape
  (make-shape [           (pos/make-pos 1 0) (pos/make-pos 2 0)
               (pos/make-pos 0 1) (pos/make-pos 1 1)]))

(def z-shape
  (make-shape [(pos/make-pos 0 0) (pos/make-pos 1 0)
                          (pos/make-pos 1 1) (pos/make-pos 2 1)]))

(def j-shape
  (make-shape [          (pos/make-pos 1 0)
                         (pos/make-pos 1 1)
                         (pos/make-pos 1 2)
               (pos/make-pos 0 2)]))

(def t-shape
  (make-shape [(pos/make-pos 0 0) (pos/make-pos 1 0) (pos/make-pos 2 0)
                          (pos/make-pos 1 1)]))
(def shapes
  [l-shape square-shape i-shape s-shape z-shape j-shape t-shape])

(defn random-shape! []
  (get shapes (rand-int (count shapes))))

(comment
  (count shapes)
  (rand-int (count shapes))
  (get shapes (rand-int (count shapes)))
  (shape-pos-ls l-shape)
  ;; => [(0 0) (0 1) (0 2) (1 2)]
  ;; calculate width
  (reduce max (map pos/pos-x (shape-pos-ls l-shape)))
  ;; => 1
)

