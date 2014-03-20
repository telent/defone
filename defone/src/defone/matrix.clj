(ns defone.matrix)

;; matrix multiplication in baby steps

(defn transpose [a]
  (apply map vector a))

(defn- multiply-cell [row-from-a col-from-b]
  (apply + (map * row-from-a col-from-b)))

(defn- multiply-row [row-from-a b]
  (map #(multiply-cell row-from-a %) (transpose b)))

(defn multiply [a b]
  (map #(multiply-row % b) a))

(assert (= '((2 2)
             (2 2))
           (multiply
            [[2 0]
             [0 2]]
            [[1 1]
             [1 1]])))


;;; transformations

(defn translate [tx ty tz]
  [[1 0 0 tx]
   [0 1 0 ty]
   [0 0 1 tz]
   [0 0 0 1]])

(defn rotate-z [angle]
  (let [c (Math/cos angle)
        s (Math/sin angle)]
    [[ c     s    0    0]
     [(- s)  c    0    0]
     [ 0     0    1    0]
     [ 0     0    0    1]]))

(defn scale [xs ys zs]
  [[xs 0  0  0]
   [ 0 ys 0  0]
   [ 0 0  zs 0]
   [ 0 0  0  1]])
