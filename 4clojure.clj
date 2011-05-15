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

;; #61-map-construct
(def map-construct
     (fn [xs ys]n
       (loop [k (seq xs)
              v (seq ys)
              the-map {}]
         (if (and k v)
           (recur (next k)
                  (next v)
                  (assoc the-map (first k)(first v)))
           the-map))))

;; #62 Iterate
(def my-iterate
     (fn this [f x]
       (cons x (lazy-seq (this f (f x))))))

;; #66 Greatest Common Divisior
(def gcd
     (fn [x y]
       "This one short circuits"
       ( if  (= 0 (rem (max x y) (min x y)))
         (min x y)
         (loop [n 1
                greatest 1]
           (if (> n (/ (min x y) 2))
             greatest
             (if (= 0 (rem x n) (rem y n))
               (recur (inc n) n)
               (recur (inc n) greatest)))))))

;; #83 Half-Truth
(def half-truth 
     (fn [& n]
       (= 2 (count (distinct n)))))

;; #65 Intro to Reduce
(def safe-empty-add-reduce
     (fn
       "Learned var arg functions to handle empty sequences to reduce"
       ([] 0)
       ( [s n] (+ s n))))

;; #63 Group a Sequence
(def group-seq
     (fn [f coll]
       "Does this really need the reverse?"
       (reduce (fn [map item]
                 (let [result (f item)
                       entry (map result)]
                   (assoc map result (cons item entry)))) {} (reverse coll))))
