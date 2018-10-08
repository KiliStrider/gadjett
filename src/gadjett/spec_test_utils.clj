(ns gadjett.spec-test-utils
  (:require [clojure.pprint :as pprint]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [is]]
            [clojure.spec.test.alpha :as stest]))


(defn summarize-results' [spec-check]
  (map (comp #(pprint/write % :stream nil) stest/abbrev-result) spec-check))

(defn check' [spec-check]
  (is (nil? (-> spec-check first :failure))
      (summarize-results' spec-check)))

(defn check-n
  "call stest/check with num-tests equals `n`
   (check-n fn n)"
  [fn n]
  (stest/check fn {:clojure.spec.test.check/opts {:num-tests n}}))

(defn check-100
  "call stest/check with num-tests equals `100`
   (check-100 fn)"
  [fn]
  (check-n fn 100))
