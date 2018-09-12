(defproject viebel/gadjett "0.5.3"
  :description "Inspector tools for clojure[script]"
  :url "https://github.com/viebel/gadjett"
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.227"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.async "0.3.442"]]
  :profiles {:codox {:dependencies [[viebel/codox-klipse-theme "0.0.5"]]
                     :plugins [[lein-codox "0.10.2"]]}
             :dev {:dependencies [[org.clojure/tools.nrepl "0.2.13"]
                                  [midje "1.9.0-alpha6"]
                                  [org.clojure/test.check "0.9.0"]
                                  [com.velisco/herbert "0.7.0"]
                                  [figwheel-sidecar "0.5.6"]]
                   :plugins [[lein-cljsbuild "1.1.5"]
                             [lein-figwheel "0.5.0-6"]
                             [lein-midje "3.2"]]}}
  :figwheel {:server-port 2512}
  :source-paths ["src" "script" "try"]
  :codox {
                                        ; :language :clojurescript
          :source-uri "https://github.com/viebel/gadjett/blob/master/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :source-paths ["src"]
          :output-path "docs"
          :themes [:default [:klipse {:klipse/external-libs  "src"
                                      :klipse/bundled-ns-ignore-regexp #"/gadjett\..*/"
                                      :klipse/cached-macro-ns-regexp #"/gadjett\..*|cljs\..*/"
                                      :klipse/cached-ns-regexp #"/gadjett\..*|cljs\..*/"
                                      :klipse/cached-ns-root "./cache-cljs"
                                      :klipse/require-statement "(ns test.gadjett
                                                         (:require
                                                         [gadjett.linear :as glinear :refer [intersection-point-func linear-equation intersection-point find-below-and-above-y linear-equation-func highest-below-x linear-y linear-x-func log-x-intersection-point interpolate-linear-y linear-y-func intersection-point-from-2-lines-points lowest-above-x find-keys-with-value log-x-linear-y lowest-above-y log-x-linear-equation calc-interpolated-values linear-x interpolate-linear-x below-and-above-y highest-below-y find-keys-with-values-in]]
                                                         [gadjett.collections :as gcoll :refer [compact unflatten-keys sequence->map sequence-of-maps->map =without-keys? dissoc-in split-by-predicate-positions mapify submap? positions filter-map vec->map assoc-cyclic infinity subsequence edn-zip fix-blank-lines map-nested-vals map-to-object deep-merge deep-merge-with mean map-object-kv flatten-keys flatten-keys* compactize-map compactize-map-recursive partition-between take-from-map map-with-index map-object-with-key deep-merge* range-till-end nearest-of-seq branches-and-leaves remove-ending-comments remove-blank-lines filter-branches out-of-bound? map-2d-vec split-by-predicate collify abs append-cyclic map-2d-vec-kv max-and-min index-of nearest-of-ss map-reverse-hierarchy map-object my-replace join-them]]))"}]]}
  :cljsbuild {
              :builds
              {
               :figwheel {
                          :figwheel true
                          :source-paths ["src" "try"]
                          :compiler {:main "gadjett.test"
                                     :asset-path "js"
                                     :output-to "resources/public/js/main.js"
                                     :output-dir "resources/public/js"
                                        ;:elide-asserts true
                                     :verbose true}}
               :dev {
                     :source-paths ["src"]
                     :compiler {
                                :output-to "resources/main.dev.js"
                                :output-dir "resources/public/dev"
                                :optimizations :none
                                :pretty-print true}}}})
