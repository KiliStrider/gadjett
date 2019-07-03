(ns gadjett.test-utils
  (:use [midje.sweet]
        [gadjett.utils]))

(def test-collection [{:a "aa0" :b "bb0" :c "cc1" :d "dd0"}
                      {:a "ab0" :b "bb0" :c "cc0" :d "dd1"}
                      {:a "ac0" :b "bb0" :c "cc0" :d "dd2"}
                      {:a "aa0" :b "bb0" :c "cc0" :d "dd3"}])

(facts "adv-sort-by tests"
       (tabular
         (fact "adv-sort-by"
               (adv-sort-by ?keys-order test-collection)
               =>
               (sort-by (juxt :a :b :c) ?cmp-func test-collection))
         ?keys-order ?cmp-func
         [:a :descending :b :ascending :c :ascending] (fn [[xa xb xc] [ya yb yc]] (compare [ya xb xc] [xa yb yc]))
         [:a :ascending :b :ascending :c :ascending] #(compare %1 %2)
         [:a :descending :b :descending :c :descending] #(compare %2 %1)))
