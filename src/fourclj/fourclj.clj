(ns fourclj.fourclj)

;; #26 Fibonacci
(def my-fib
     #(take % 
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
       "Ripe for refactoring!!!" ()
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

;; #54 Partition a Sequence
(def partition-sequence
     (fn [n coll]
       (loop [s coll
              ret []]
         (let [next-part (take n s)]
           (if (= n (count next-part))
             (recur (drop n s) (conj ret next-part))
             ret)))))

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
;; #58
(def function-comp
     (fn [& fns]
       (fn [& args]
         (let [reversed-functions (reverse fns)]
           (loop [funs (rest reversed-functions)
                  val (apply (first reversed-functions) args)]
             (if (first funs)
               (recur (rest funs) ((first funs) val))
               val))))))

;; #59
(def juxtaposition
     (fn [& fns]
       (fn [& args]
         (do
           (println args)
           (println fns)
           (reduce #(conj %1 (apply %2 args)) [] fns)))))

;;#60
(def seq-reducitons
     (fn this
       ([f coll]
          (lazy-seq
           (if-let [s (seq coll)]
             (this f (first s) (rest s))
             (list (f)))))
       ([f init coll]
          (cons init
                (lazy-seq
                 (when-let [s (seq coll)]
                   (this f (f init (first s)) (rest s))))))))


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

;; #67 Primes
;; how to recur as below from within, had to crapily use range instead
;; of range
(def primes-up
     (fn [n]
       (take n (lazy-cat
                (cons 2 ((fn rest-primes [next-prime]
                           (let [potential-divisiors (range 2 next-prime)
                                 n (inc next-prime)]
                             (if (some #(zero? (rem next-prime %)) potential-divisiors)
                               (recur n)
                               (lazy-cat (cons next-prime (rest-primes n)))))) 3))))))

(def primes
     (lazy-cat
      (cons 2 ((fn rest-primes [next-prime]
                 (let [potential-divisiors (take-while #(<= (* % %) next-prime) primes)
                       n (inc next-prime)]
                   (if (some #(zero? (rem next-prime %)) potential-divisiors)
                     (recur n)
                     (lazy-cat (cons next-prime (rest-primes n)))))) 3))))


;; 69 merge-with
(def merge-with-fn
     (fn [f & maps]
       (when (first maps)
         (letfn [(merge-entry [m e] 
                             (let [k (key e)
                                   v (val e)]
                               (if (contains? m k)
                                 (assoc m k (f (get m k) v))
                                 (assoc m k v))))
               (merge2 [m1 m2]
                        (reduce merge-entry (or m1 {}) (seq m2)))]
           (reduce merge2 maps)))))

;; #70
(def word-sort
     (fn [s]
       (sort-by #(.toLowerCase %)
                (re-seq #"[A-Za-z]+" s))))



;; #74 perfect squares
(def perfect-squares
     (fn [coll]
       (apply str
              (interpose
               ","
               (filter
                #(let [sqrt (Math/sqrt (Integer/parseInt %1))]
                   (= 0 (- sqrt (Math/floor sqrt))))
                (re-seq #"[0-9]+" coll))))))


;; #80 Perfectnumbers
(def perfect-number?
     (fn [n]
       (let [divisiors (filter #(zero? (rem n %)) (range 1 n))]
         (= n (apply + divisiors)))))


;; #81 Intersection
(def intersection
     (fn [coll1 coll2]
       (if (< (count coll1) (count coll2))
         (recur coll2 coll1)
         (reduce #(if (contains? coll2 %2)
                    (conj %1 %2)
                    %1)
                 #{}
                 coll1))))

;; #83 Half-Truth
(def half-truth 
     (fn [& n]
       "Could short circuit, but feels slick"
       (= 2 (count (distinct n)))))



























