(ns user)

;; #26-Fibonacci
(def my-fib # (take % 
                    (map first 
                         (iterate (fn [[x y]] [y (+ x y)]) [1 1]))))
;; #38 -Max
(def my-max
     (fn this ([x] x)
              ([x y] (if (> x y) x y))
              ([x y & more] (reduce this (this x y) more))))

;; #39-Interleave
(def my-interleave
     (fn this [x y]
                     (when (and (first x) (first y))
                       (cons (first x) (cons (first y) (this (rest x)(rest y)))))))

;; #40-Interpose
(def my-interpose
     (fn this [c xs]
                    (when-let [n (first xs)]
                      (if (second xs)
                        (cons n (cons c (this c (rest xs)))) 
                        (cons n (this c (rest xs)))))))

;; #41-Drop Every Nth Item
(def drop-every-nth
     (fn [xs n]
       (filter (complement nil?)
               (map-indexed 
                #(if (not= 0 (rem (inc %1)  n))%2)
                    xs))))
;; #42-factorial
(def factoral
     (fn [n]
       #(apply * (range 1 (inc %)))))

;; #43-Reverse interleave
(def reverse-interleave (fn [n coll] 
  (map #(take-nth n (drop % coll)) (range n))))

;; #49-Split Sequence
(def split-sequence
     (fn [n coll]
       [(take n coll) (drop n coll)]))
