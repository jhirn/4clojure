(ns user)

;; #26 Fibonacci
(def my-fib #(take % 
                   (map first 
                        (iterate (fn [[x y]] [y (+ x y)]) [1 1]))))
;; #38 Max
(def my-max
     (fn this ([x] x)
       ([x y] (if (> x y) x y))
       ([x y & more] (reduce this (this x y) more))))

;; #39 Interleave
(def my-interleave
     (fn this [x y]
       (when (and (first x) (first y))
         (cons (first x) (cons (first y) (this (rest x)(rest y)))))))

;; #40 Interpose
(def my-interpose
     (fn this [c xs]
       (when-let [n (first xs)]
         (if (second xs)
           (cons n (cons c (this c (rest xs)))) 
           (cons n (this c (rest xs)))))))

;; #41 Drop Every Nth Item
(def drop-every-nth
     (fn [xs n]
       (filter (complement nil?)
               (map-indexed 
                #(if (not= 0 (rem (inc %1)  n))%2)
                xs))))
;; #42 Factorial
(def factoral
     (fn [n]
       #(apply * (range 1 (inc %)))))

;; #43 Reverse interleave
(def reverse-interleave (fn [n coll] 
                          (map #(take-nth n (drop % coll)) (range n))))

;; #44 Rotate Sequence
(def rotate-seq
     (fn [n coll]
       (let [c (count coll)]
         (take c (drop (mod n c) (cycle coll))))))

;; #46 flipping out
(def flip-args
     (fn this [f]
       (fn [x y] (f y x))))

(defn
  ^{:doc "This def is so meta"
    :test (fn []
            (assert ((flip-args-so-meta >) 7 8) )
            (assert (= 3 ((flip-args-so-meta nth) 2 [1 2 3 4 5])))
            (assert (= 4 ((flip-args-so-meta quot) 2 8)))
            (assert (= [1 2 3] ((flip-args-so-meta take) [1 2 3 4 5] 3))))}
  flip-args-so-meta [f]
  (fn [x y]
    (f y x)))

(test flip-args-so-meta)

;; #49-Split Sequence
(def split-sequence
     (fn [n coll]
       [(take n coll) (drop n coll)]))

;; #50 Split by type
;; This one taught me lack of need for reverse via conj on vector
(def split-by-type
     (fn [coll]
       (vals (reduce #(let [type-vector (get %1 (type %2) []) ]
                        (assoc %1 (type %2) (conj type-vector %2)))
                     {}
                     coll))))

;; #53 Longest Increasing Sub-Seq
(def longest-inc-sub-seq
     (fn [coll]
       "Ripe for refactoring!!!"
       (loop [s coll
              current (first s)
              current-run [(Integer/MAX_VALUE)]
              longest-run []]
         (if (nil? current)
           longest-run
           (let [new-run (if (= (inc (last current-run)) current)
                           (conj current-run current)
                           [current])
                 new-longest (if (and (> (count new-run) 1)
                                      (> (count new-run) (count longest-run)))
                               new-run
                               longest-run)]
             (recur (next s)
                    (first (next s))
                    new-run
                    new-longest))))))

;; #55 Count Ocurences
(def count-occurences
     (fn [coll]
       "Use the cool transients from (fn frequencies)"
       (reduce (fn [count-map item]
                 (assoc count-map item ( inc (get count-map item 0))))
               {}
               coll)))

;; #55 Find Distinct Items
(def distinct-items
     (fn [coll]
       "radically wack compared to distinct"
       (reduce
        #(if (nil?(some #{%2} %1))
           (conj %1 %2)
           %1) [] coll)))

;; #61 Map-construct
(def map-construct
     (fn [xs ys]
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


;; #63 Group a Sequence
(def group-seq
     (fn [f coll]
       "Does this really need the reverse?"
       (reduce (fn [map item]
                 (let [result (f item)
                       entry (map result)]
                   (assoc map result (cons item entry)))) {} (reverse coll))))

;; #65 Intro to Reduce
(def safe-empty-add-reduce
     (fn
       ([]
          "Learned var arg functions to handle empty sequences to reduce"
          0)
       ( [s n]
           "Also learned where doc strings go on var arg functions"
           (+ s n))))

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
       "Could short circuit, but feels slick"
       (= 2 (count (distinct n)))))



























