(ns user)

;; #26-Fibonacci
(def my-fib # (take % 
                    (map first 
                         (iterate (fn [[x y]] [y (+ x y)]) [1 1]))))
;; #38 -Max
(def my-max (fn this ([x] x)
              ([x y] (if (> x y) x y))
              ([x y & more] (reduce this (this x y) more))))

;; #39-Interleave
(def my-interleave (fn this [x y]
                     (when (and (first x) (first y))
                       (cons (first x) (cons (first y) (this (rest x)(rest y)))))))

;; #40-Interpose
(def my-interpose (fn this [c xs]
                    (when-let [n (first xs)]
                      (if (second xs)
                        (cons n (cons c (this c (rest xs)))) 
                        (cons n (this c (rest xs)))))))
