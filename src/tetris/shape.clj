(ns tetris.shape
  (:require [tetris.position :as pos]))

;; shapes
(defn make [pos-ls] {:pos-ls pos-ls})

(defn pos-ls [shape] (get shape :pos-ls))

(defn eq? [shape1 shape2]
  (pos/pos-ls-eq? (pos-ls shape1) (pos-ls shape2)))

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
  ([shape]
   (let [min-pos (pos/min-pos (pos-ls shape))]
     (normalize shape min-pos)))
  ([shape pos]
   (make
    (->>
     (pos-ls shape)
     (map (fn [p] (pos/sub p pos)))
     (sort pos/compare-pos)))))

(defn flip
  "Returns the shape flipped by 90' anti-clockwise"
  [shape]
  (let [pos-ls (pos-ls shape)
        flipped-pos-ls (map pos/flip-90 pos-ls)
        shift (pos/abs-pos (pos/min-pos (cons (pos/make 0 0) flipped-pos-ls)))
        flipped-shifted-pos-ls (map (partial pos/add shift) flipped-pos-ls)]
    (make flipped-shifted-pos-ls)))

(defn shift
  "Returns the difference on the height and width between shape1 and shape2"
  [shape1 shape2]
  (let [ width-diff (- (width shape1) (width shape2))
        height-diff (- (height shape1) (height shape2))]
    (pos/make width-diff height-diff)))

(defn remove-row
  [shape row]
  (let [pos-to-remove (filter (fn [p] (= (pos/y p) row)) (pos-ls shape))]
    (if (empty? pos-to-remove)
      shape
      (let [pos-above-row? (fn [p] (< (pos/y p) row))
            pos-below-row? (fn [p] (> (pos/y p) row))
            pos-above-row (filter pos-above-row? (pos-ls shape))
            pos-below-row (filter pos-below-row? (pos-ls shape))
            down-by-1 (fn [p] (pos/make (pos/x p) (inc (pos/y p))))
            down-by-1-above-row (map down-by-1 pos-above-row)]
        (make (concat down-by-1-above-row pos-below-row))))))


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

  (flip l)
  ;; => {:pos-ls ((2 0) (1 0) (0 0) (0 1))}
  ;; => {:pos-ls ((0 0) (1 0) (2 0) (2 1))}

  (vals (group-by even? [1 2 3 4]))
  ;; => ([1 3] [2 4])

  (#{1 2 3} 1)
  ;; => 1

  )

