(ns tetris.position)

;; positions
(defn make [x y] (list x y))
;; pos-x
(defn x [pos] (first pos))
;; pos-y
(defn y [pos] (last pos))

(defn add [pos1 pos2]
  (make
   (+ (x pos1) (x pos2))
   (+ (y pos1) (y pos2))))

(defn sub [pos1 pos2]
  (make (- (x pos1) (x pos2))
        (- (y pos1) (y pos2))))

(defn eq? [pos1 pos2]
  (and (= (x pos1) (x pos2))
       (= (y pos1) (y pos2))))

(defn in-pos-ls? [pos pos-ls]
  (some (fn [p] (eq? pos p)) pos-ls))

(defn pos-ls-intersect? [pos-ls1 pos-ls2]
  (some (fn [p1] (in-pos-ls? p1 pos-ls2)) pos-ls1))

(defn rand-pos [max-pos]
  ;; returns a random position where max-pos is the upper-bound excluded
  ;; the y is fixed to 0, only the x is random
  (make (rand-int max-pos) 0))

(defn min-pos
  "Returns the minimum position calculate by combining the min x and the min y"
  ([pos-ls]
   (make
    (apply min (map x pos-ls))
    (apply min (map y pos-ls))))
  ([pos-ls min-val]
   (make
    (abs (apply min (cons min-val (map x pos-ls))))
    (abs (apply min (cons min-val (map y pos-ls)))))))

(defn compare-pos
  "Compares 2 positions to sort them"
  [pos1 pos2]
  (let [factor 2]
    (+ (* (- (x pos1) (x pos2)) factor)
       (- (y pos1) (y pos2)))))

(defn flip
  "Returns a position with the coordinates x and y flipped"
  [pos]
  (make (y pos) (x pos)))

(defn flip-90
  "Applies a 90' anti clockwise rotation to the vector represented by the position"
  [pos]
  (make (* -1 (y pos)) (x pos)))

(defn contiguos?
  "Returns true if 2 positions are next to each other"
  [pos1 pos2]
  (let [xs (map x [pos1 pos2])
        ys (map y [pos1 pos2])
        same-and-diff-by-1 (fn [[x1 x2] [y1 y2]]
                             (and (= x1 x2)
                                  (= (abs (- y1 y2)) 1)))]
    (or (same-and-diff-by-1 xs ys)
        (same-and-diff-by-1 ys xs))))

(defn contiguos-to-ls?
  "Returns true if pos is contiguos to any position in pos-ls"
  [pos pos-ls]
  (some (fn [p] (contiguos? p pos)) pos-ls))

(defn find-contiguous-group
  "Find the group of contiguous positions to pos in pos-ls"
  ([pos pos-ls]
   (find-contiguous-group [pos] [] pos-ls))
  ([pos-ls]
   (find-contiguous-group [(first pos-ls)] [] (rest pos-ls)))
  ([continguos-ls rest-ls pos-ls]
   (if (empty? pos-ls)
     (list continguos-ls rest-ls)
     (let [p (first pos-ls)
           rest-pos-ls (rest pos-ls)]
       (if (contiguos-to-ls? p continguos-ls)
         (find-contiguous-group (cons p continguos-ls)
                                rest-ls
                                rest-pos-ls)
         (find-contiguous-group continguos-ls
                                (cons p rest-ls)
                                rest-pos-ls))))))

(defn find-contiguous
  "Given a list of positions pos-ls, return all the groups of contiguos positions"
  ([pos-ls] (find-contiguous [] pos-ls))
  ([groups pos-ls]
   (if (empty? pos-ls)
     groups
     (let [[contiguous-ls rest-ls]
           (find-contiguous-group (first pos-ls) (rest pos-ls))]
       (find-contiguous (cons contiguous-ls groups) rest-ls)))))

(comment
  (make (rand-int 10) (rand-int 10))
  ;; => (2 7)
  (def other-pos-ls '((0 2) (0 3) (0 4) (1 4)))
  other-pos-ls
  (def piece-pos-ls-after-move '((0 1) (0 2) (1 1) (1 2)))
  piece-pos-ls-after-move
  (pos-ls-intersect? other-pos-ls piece-pos-ls-after-move)

  (contiguos? '(1 0) '(2 0))
  ;; => true

  (contiguos? '(1 0) '(1 3))
  ;; => false

  (contiguos-to-ls? '(1 0) '((2 0) (2 1) (3 0) (3 4) (4 2) (4 3) (4 4)))
  ;; => true

  (def pos-ls '((1 0) (2 0) (2 1) (3 0) (3 4) (4 2) (4 3) (4 4)))

  (find-contiguous-group pos-ls)
  ;; => (((3 0) (2 1) (2 0) (1 0)) ((4 4) (4 3) (4 2) (3 4)))


  (def groups
    (find-contiguous pos-ls))

  groups
  ;; => (((3 4) (4 2) (4 3) (4 4)) ((3 0) (2 1) (2 0) (1 0)))

  (sort compare-pos '((0 2) (1 0) (1 1) (1 2)))
  ;; => ((0 2) (1 0) (1 1) (1 2))

  (flip (make 0 1))
  ;; => (1 0)
  )
