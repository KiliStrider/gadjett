(ns gadjett.check-collections
  (:use [gadjett.collections])
  (:require [miner.herbert.generators :as hg]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]))



(defspec check-max-and-min 100
  (hg/property (fn [v] 
                 (let [[the-max the-min] (max-and-min v)]
                   (and
                    (= the-min (apply min v))
                    (= the-max (apply max v)))))
               '[int+]))

(defspec check-take-from-map 100
  (prop/for-all [m (gen/recursive-gen
                    (fn [inner] (gen/such-that not-empty (gen/map gen/int inner)))
                    gen/int)#_(gen/recursive-gen (partial gen/map gen/int) gen/boolean)
                 n gen/pos-int]
                (let [size-of-m (count (flatten-keys m))
                      expected-size (min n size-of-m)]
                  (= expected-size (count (flatten-keys (take-from-map n m)))))))

(defspec check-flatten-keys 100
  (prop/for-all [mmm (gen/such-that not-empty (gen/map gen/keyword gen/int))]
                (let [m {:a mmm}
                      res (flatten-keys m)]
                  (every? true? (map #(= (get res %) (get-in m %)) (keys res))))))

(defspec check-append-cyclic 100
  (hg/property (fn [v x]
                 (let [res (append-cyclic v x)]
                   (and (= (count res) (count v))
                        (= x (last res))
                        (= (first res) (second v))
                        (= (drop-last 1 res) (rest v)))))
               '[int+ 2] 'int))


(defn dist [a b]
  (abs (- a b)))

(defn distances [coll x]
  (map #(dist x %) coll))

(defn min-dist [coll x]
  (if (empty? coll) 0
      (apply min (distances coll x))))

(defspec check-nearest-of-seq 30
  (prop/for-all [a (gen/vector gen/int)
                 b (gen/vector gen/int)]
                (let [res (nearest-of-seq a b)]
                  (= (map #(min-dist a %) b)
                     (map dist res b)))))
;;;

#(defspec check-unflatten-keys 100
   (prop/for-all [m (gen/map (gen/such-that not-empty (gen/vector gen/int)) gen/int)]
                 (= m (flatten-keys (unflatten-keys m)))))

(defspec check-flatten-keys 100
  (prop/for-all [m (gen/recursive-gen 
                    (fn [inner] (gen/such-that not-empty (gen/map gen/int inner)))
                    gen/int)]
                (= m (unflatten-keys (flatten-keys m)))))

(defspec check-out-of-bounds 100
  (prop/for-all [v (gen/vector gen/int)
                 i gen/int]
                (= (out-of-bound? v i) (not (get v i)))))

(defspec check-filter-map-nil 100
  (prop/for-all [m (gen/map gen/keyword gen/int)]
                (= (filter-map (comp not nil?) m) (compactize-map m))))

(defspec check-substr-start-0 10
  (prop/for-all [s gen/string]
                (= (substr s 0) s)))

(defspec check-substr-start-with-large-start 10
  (prop/for-all [s gen/string]
                (prop/for-all
                 [n (gen/large-integer* {:min 100 #_(inc (count s))})]
                 (println "s: " s " n: " n)
                 (= (substr s 0 n) s))))

(defspec check-order-by 100
         (hg/property (fn [v]
                        (let [keyfn-direction-pairs [#(mod % 3) :asc #(mod % 2) :desc #(mod % 5) :asc #(mod % 19) :desc]
                              keyfns (take-nth 2 keyfn-direction-pairs)
                              order-by-v (order-by keyfn-direction-pairs v)
                              sort-by-v (sort-by
                                          (apply juxt keyfns)
                                          (fn [[x1 x2 x3 x4] [y1 y2 y3 y4]]
                                            (compare [x1 y2 x3 y4] [y1 x2 y3 x4]))
                                          v)]
                          (and
                            (= order-by-v sort-by-v))))
                      '[int+]))
