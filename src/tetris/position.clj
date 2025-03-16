(ns tetris.position)

;; positions
(defn make-pos [x y] (list x y))
;; pos-x
(defn pos-x [pos] (first pos))
;; pos-y
(defn pos-y [pos] (last pos))

(defn pos-add [pos1 pos2]
  (make-pos
   (+ (pos-x pos1) (pos-x pos2))
   (+ (pos-y pos1) (pos-y pos2))))

(defn pos-eq? [pos1 pos2]
  (and (= (pos-x pos1) (pos-x pos2))
       (= (pos-y pos1) (pos-y pos2))))

(defn pos-in-pos-ls? [pos pos-ls]
  (some (fn [p] (pos-eq? pos p)) pos-ls))

(defn pos-ls-intersect? [pos-ls1 pos-ls2]
  (some (fn [p1] (pos-in-pos-ls? p1 pos-ls2)) pos-ls1))

(defn rand-pos [max-pos]
  ;; returns a random position where max-pos is the upper-bound excluded
  ;; the y is fixed to 0, only the x is random
  (make-pos (rand-int max-pos) 0))

(comment
  (make-pos (rand-int 10) (rand-int 10))
  ;; => (2 7)
  (def other-pos-ls '((0 2) (0 3) (0 4) (1 4)))
  other-pos-ls
  (def piece-pos-ls-after-move '((0 1) (0 2) (1 1) (1 2)))
  piece-pos-ls-after-move
  (pos-ls-intersect? other-pos-ls piece-pos-ls-after-move)
)
