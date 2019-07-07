(ns gadjett.test-collections
  (:use [midje.sweet]
        [gadjett.collections]))

(defn set= [a b]
  (= (set a) (set b)))

(facts "Collections (1)"
       (fact "sequence->map"
             (sequence->map  [10 20 30]) => {0 10 1 20 2 30})
       (fact "sequence-of-maps->map"
             (sequence-of-maps->map [{:key :price :value 19}
                                     {:key :quantity :value 100}]
                                    :key :value) => {:price 19 :quantity 100})
       (tabular
        (fact "partition-between" (partition-between ?pred ?coll) => ?res)
        ?pred ?coll ?res
        (fn [a b] (> (- b a) 2)) [1 4 5 8 9 10 15 20 21] [[1] [4 5] [8 9 10] [15] [20 21]]
        (fn [a b] (> a b)) [1 2 4 9 4 6 8 2] [[1 2 4 9] [4 6 8] [2]]
        )
       (tabular
        (fact "branches-and-leaves" (branches-and-leaves ?coll) => ?res)
        ?coll ?res
        {} {:branches [{}] :leaves []}
        [] {:branches [[]] :leaves []}
        [{:a 5} {:b 7}] {:branches [[{:a 5} {:b 7}] {:a 5} {:b 7}] :leaves [5 7]}
        {:a {:b 7}} {:branches [{:a {:b 7}} {:b 7}] :leaves [7]}
        )
       (tabular
        (fact "filter-branches" (filter-branches ?coll ?pred) => (partial set= ?res))
        ?coll ?pred ?res
        {} :a '()
        {:a {:b 7}} :c '()
        [{:a 5} {:b [7 6]}] :b '({:b [7 6]})
        {:a {:b 7} :c {:l 7}} :b '({:b 7})
        {:a {:b {:id 5} :c 9} :id 7 :j {:id 8}} :id '({:a {:b {:id 5} :c 9} :id 7 :j {:id 8}} {:id 8} {:id 5})
        )
       (tabular
        (fact "submap?" (submap? ?m1 ?m2) => ?res)
        ?m1 ?m2 ?res
        {} {} true
        {} {:a1 1} true
        {:a 1} {:a 1} true
        {:a 1} {} false
        {:a 1} {:a 1 :b 2} true
        {:a 1} {:a 2 :b 1} false)
       (tabular
        (fact "mapify" (mapify ?f ?s) => ?res)
        ?s ?f ?res
        [] - {}
        [1 2 3] - {-1 1 -2 2 -3 3}
        [{}] :a {nil {}}
        [{:a 1} {:a 2}] :a {1 {:a 1} 2 {:a 2}}
        )
       (tabular
        (fact "filter-map" (filter-map ?f ?m) => ?res)
        ?f ?m ?res
        identity {:a 1 :b 2} {:a 1 :b 2}
        (comp not-empty str) {:a 1 :b nil :c ""} {:a 1}
        (comp not nil?) {:a 1 :b nil :c ""} {:a 1 :c ""})
       (tabular
        (fact "=without-keys?"
              (=without-keys? ?a ?b ?kl) => ?res)
        ?a ?b ?kl ?res
        {:a 1 :b 2} {:a 2 :b 2} [:b] false
        {:a 1 :b 2} {:a 1 :b 3} [:b] true
        {} {:b 3} [:b] true
        {:a 1 :b 2 :c 3} {:a 1 :b 4 :c 5} [:b :c] true
        )
       (tabular
        (fact "deep-merge" (deep-merge ?a ?b) => ?res)
        ?a ?b ?res
        {:a {:b {1 'a' 2 'b'}}} {:a {:b {1 'I'}}} {:a {:b {1 'I' 2 'b'}}}
        {:a {:b {1 'a'}}} {} {:a {:b {1 'a'}}}
        {:a {:b {1 'a'}}} nil {:a {:b {1 'a'}}}
        )
       (tabular
        (fact "join-them a single collection" (join-them [?fn] [?coll]) => ?coll)
        ?fn ?coll 
        :a [{:a 1 :b 2}]) 
       (tabular
        (fact "join-them - two  collections" (join-them [?fn-a ?fn-b] [?a ?b]) => ?res)
        ?a            ?fn-a ?b            ?fn-b ?res
        [{:a 1 :b 2}] :a    [{:x 1 :y 2}] :x    [{:x 1 :a 1 :b 2 :y 2}])       
       (tabular
        (fact "deep-merge-with" (deep-merge-with concat ?a ?b) => ?res)
        ?a ?b ?res
        {:a []} {} {:a []}
        {:a [1]} {:a [2]} {:a '(1 2)}
        {:x {:a [1]}} {:x {:a [2]}} {:x {:a '(1 2)}}
        )
       (tabular
        (fact "unflatten-keys" (unflatten-keys ?m) => ?res)
        ?m ?f ?res
        {[:a :b :c] "1" [:d :e :f] true} {:a {:b {:c "1"}} :d {:e {:f true}}}
        )
       )

(facts "Collections (2)"
       (tabular
        (fact "flatten-keys"
              (flatten-keys ?a) => ?res)
        ?a ?res
        {} {}
        {:a "aaa" :b {:c "param" "d" "txt"}} {[:a] "aaa" [:b :c] "param" [:b "d"] "txt"}
        )
       (fact "map-with-index"
             (map-with-index ["a" "b" "c"] :idx :val) =>  '({:idx 0, :val "a"} {:idx 1, :val "b"} {:idx 2, :val "c"}))
       (tabular
        (fact "map-reverse-hierarchy"
              (map-reverse-hierarchy ?in) => ?out)
        ?in ?out
        {} {}
                                        ;{:a [1 2] :b [1 3]} {1 [:a :b] 2 [:a] 3 [:b]}
        {:L {10 100 20 0} :R {10 50 30 0}} {10 {:L 100 :R 50} 20 {:L 0} 30 {:R 0}}
        {:L {10 100 20 0} :R {10 50 30 0} :A {}} {10 {:L 100 :R 50} 20 {:L 0} 30 {:R 0}}
        {:L {10 100 20 0} :R {10 50 30 0} :A {10 99}} {10 {:A 99 :L 100 :R 50} 20 {:L 0} 30 {:R 0}}
                                        ;{:a [[1 0] [2 50]] :b [[1 10] [3 100]]} {1 {:a 0 :b 10} 2 {:a 50} 3 {:b 100}} 
        )
       (fact "map-object"
             (map-object #(* 100 %) {:a 1 :b 2 :c 3}) => {:a 100 :b 200 :c 300})
       (fact "map-object-with-key"
             (map-object-with-key #(+ %1 %2) {2 2 3 3 4 4}) => {2 4 3 6 4 8}
             (map-object-with-key #(str %1 %2) {:a 1 :b 2})=>{:a ":a1" :b ":b2"})
       (tabular
        (fact "mean" (mean ?a) => ?b)
        ?a ?b
        [1 2 3] 2
        [] 0
        )
       (fact "vec->map"
             (vec->map []) => {}
             (vec->map [[:a 1] [:b 3]]) => {:a 1 :b 3}
             )

       (fact "append-cyclic"
             (append-cyclic [1 2 3] 4) => [2 3 4])
       (tabular
        (fact "assoc-cyclic" (assoc-cyclic ?a ?b ?c) => ?d)
        ?a ?b ?c ?d
        {} :a 1  {}
        {:a 1} :a 2 {:a 2}
        {:b 1} :a 2 {:a 2}
        {:b 3 :a 2} :b 1 {:a 2 :b 1}
        {:a 2 :b 3} :b 1 {:a 2 :b 1}
        {:b 3 :a 2} :a 1 {:a 1 :b 3}
        {:a 2 :b 3} :a 1 {:a 1 :b 3})
       (tabular
        (fact "assoc-cyclic with n" (assoc-cyclic ?a ?b ?c 2) => ?d)
        ?a ?b ?c ?d
        {} :a 1  {:a 1}
        {:a 1} :a 2 {:a 2}
        {:b 1} :a 2 {:b 1 :a 2}
        {:a 2 :b 3} :a 1 {:a 1 :b 3})

       (fact "max-and-min"
             (max-and-min [1 2 3]) => [3 1]
             )
       (fact "map-to-object"
             (map-to-object #(+ 2 %) [1 2 3]) => {1 3 2 4 3 5}
             )
       (tabular
        (fact "nearest-of-ss" (nearest-of-ss (apply sorted-set ?set) ?elem) => ?res)
        ?set ?elem ?res
        [1 2 3] 1.2 1
        [1 2 3] 0.2 1
        [1 2 3] 9.2 3
        )

       (fact "nearest-of-seq"
             (nearest-of-seq [1 2 3] [1.2 0.2 9.2]) => [1 1 3])
       (tabular
        (fact "positions" (positions ?coll :max-val ?max) => ?res)
        ?coll ?max ?res
        '(10 20 10) 100 '((0 10) (10 30) (30 40))
        '(10 0 10) 100 '((0 10) (10 10) (10 20))
        '(0 20 10) 100 '((0 0) (0 20) (20 30))
        '(10 20 10) 35 '((0 10) (10 30) (30 35))
        '(30) 20 '((0 20)))
       (tabular
        (fact "split-by-predicate" (split-by-predicate ?coll (partial = 0) ?n) => (vec ?res))
        ?coll ?n ?res
        '(0 123 0 1 0 0 0 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0 0 0 1 199)  4 '((0 123 0 1) (0 0 0 0 0) (1 1) (0 0 0 0 0) (1 0 1) (0 0 0 0 0) (1 199))
        '(123 0 1 0 0 0 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0 0 0 1 199)  4 '((123 0 1) (0 0 0 0 0) (1 1) (0 0 0 0 0) (1 0 1) (0 0 0 0 0) (1 199))
        '(123 0 1 0 0 0 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0 0 0 1 199)  5 '((123 0 1) (0 0 0 0 0) (1 1) (0 0 0 0 0) (1 0 1) (0 0 0 0 0) (1 199)))
       (tabular
        (fact "split-by-predicate-positions with factor = 1" (split-by-predicate-positions ?coll (partial = 0) ?n 1) => ?res)
        ?coll ?n ?res
        '(0 123 0 1 0 0 0 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0 0 0 1 199)  4 '((0 4) (4 9) (9 11) (11 16) (16 19) (19 24) (24 26))
        '(123 0 1 0 0 0 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0 0 0 1 199)  4 '((0 3) (3 8) (8 10) (10 15) (15 18) (18 23) (23 25)))
       )
(facts "Collections (3)"
       (tabular
        (fact "my-replace"
              (my-replace ?smap ?form) => ?res)
        ?smap ?form ?res
        '{a (b c)} '() '()
        '{a (b)} '(xx) '(xx)
        '{a (b)} '(a) '(b)
        '{a (b c)} '(a) '(b c)
        '{a (b c)} '(a ()) '(b c ())
        '{a (b x) c (d e)} '(a kk c (c zz)) '(b x kk d e (d e zz))
        '{a (b)} '{a b} '{b b}
        '{aaa (AAA BB CC)} '(aaa aa bb (a aaa (bb aaa))) '(AAA BB CC aa bb (a AAA BB CC (bb AAA BB CC))))
       (tabular
        (fact "remove-blank-lines" 
              (remove-blank-lines ?in) => ?out)
        ?in ?out
        "" ""
        "  \n" ""
        "  \t\n" ""
        "   " ""
        "   \t  " ""
        "   \t   \naa\n  \nbb\n  " "aa\nbb")
       (tabular
        (fact "fix-blank-lines" 
              (fix-blank-lines ?in) => ?out)
        ?in ?out
        "" ""
        "  \n" ""
        "  \t\n" ""
        "   " ""
        "   \t  " "")
       (tabular
        (fact "remove-ending-comments"
              (remove-ending-comments ?in) => ?out)
        ?in ?out
        ""                            ""
        "aa\nbb"                      "aa\nbb"
        ";aa\nbb"                     ";aa\nbb"
        "aa\n;bb\ncc"                 "aa\n;bb\ncc"
        "aa\n;bb"                     "aa"
        "aa\n   ;bb"                  "aa"
        "aa\n\t   ;bb"                "aa"
        "aa\n;bb\n;cc;cc\n;;dd"       "aa"
        "aa\n   ;  bb\n;cc;cc\n;;dd"  "aa"
        "aa\nb;bb"                    "aa\nb;bb")
       (tabular
        (fact "collify" (collify ?in) => ?out)
        ?in ?out
        '() '()
        [] []
        '(nil) '(nil)
        'aa '(aa)
        '(aa) '(aa))
       (tabular
        (fact "substr - only start" (substr ?str ?start) => ?res)
        ?str   ?start ?res
        ""     0      ""
        ""     -1     ""
        ""     100    ""
        "a"    0      "a"
        "a"    1      ""
        "a"    100    ""
        "abcd"  0      "abcd"
        "abcd"  -2     "abcd"
        "abcd" 1     "bcd"
        "abcd" 2     "cd")
       (tabular
        (fact "substr - start and end" (substr ?str ?start ?end) => ?res)
        ?str   ?start ?end ?res
        ""     0      0    ""
        ""     -1     0    ""
        ""     100    102  ""
        "a"    0      0    ""
        "a"    0      1    "a"
        "a"    0      100  "a"
        "abcd" 0      1000 "abcd"
        "abcd"  -2    1000 "abcd"
        "abcd" 1     3     "bc"
        "abcd" 1     4     "bcd"
        "abcd" 1     1000  "bcd")
       (tabular
        (fact "compactize-map" (compactize-map ?in) => ?out)
        ?in ?out
        {}  {}
        {:a nil :b 1} {:b 1}
        {:a {:b nil}} {:a {:b nil}})
       (tabular
        (fact "compactize-map-recursive" (compactize-map-recursive ?in) => ?out)
        ?in ?out
        {}  nil
        {:a nil :b 1} {:b 1}
        {:a {:b nil}} nil
        {:a {:b nil :c 2}} {:a {:c 2}}
        {:x 1 :a {:b nil :c 2 :d {:e nil :f 2}}} {:x 1 :a {:c 2 :d {:f 2}}}))

(def test-coll-1 [{:a "aa0" :b "bb0" :c "cc1" :d "dd0"}
                  {:a "ab0" :b "bb0" :c "cc0" :d "dd1"}
                  {:a "ac0" :b "bb0" :c "cc0" :d "dd2"}
                  {:a "aa0" :b "bb0" :c "cc0" :d "dd3"}])

(def test-coll-2 [[3 4] [2 3] [5 5] [2 2] [2 4]])

(facts "order-by tests"
       (tabular
         (fact "order-by test-coll"
               (order-by ?keys-direction ?coll)
               =>
               ?test-output)
         ?keys-direction  ?coll ?test-output
         [:a :desc :b :asc :c :asc] test-coll-1 '({:a "ac0" :b "bb0" :c "cc0" :d "dd2"}
                                                  {:a "ab0" :b "bb0" :c "cc0" :d "dd1"}
                                                  {:a "aa0" :b "bb0" :c "cc0" :d "dd3"}
                                                  {:a "aa0" :b "bb0" :c "cc1" :d "dd0"})
         [:a :asc :b :asc :c :asc] test-coll-1 '({:a "aa0" :b "bb0" :c "cc0" :d "dd3"}
                                                 {:a "aa0" :b "bb0" :c "cc1" :d "dd0"}
                                                 {:a "ab0" :b "bb0" :c "cc0" :d "dd1"}
                                                 {:a "ac0" :b "bb0" :c "cc0" :d "dd2"})
         [:a :desc :b :desc :c :desc] test-coll-1 '({:a "ac0" :b "bb0" :c "cc0" :d "dd2"}
                                                    {:a "ab0" :b "bb0" :c "cc0" :d "dd1"}
                                                    {:a "aa0" :b "bb0" :c "cc1" :d "dd0"}
                                                    {:a "aa0" :b "bb0" :c "cc0" :d "dd3"})
         [first :desc second :desc] test-coll-2 '([5 5] [3 4] [2 4] [2 3] [2 2])
         [first :asc second :asc] test-coll-2 '([2 2] [2 3] [2 4] [3 4] [5 5])))
