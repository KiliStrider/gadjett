(ns gadjett.core
  (:require [cljs.analyzer]
            [gadjett.core-fn :refer [function-call-err-msg record-function-call]]
            [gadjett.collections :as collections]))

(defmacro dbg[x]
  (when *assert*
    `(let [x# ~x]
       (println (str '~x ": ") x#)
       x#)))

(defmacro breakpoint []
  '(do (js* "debugger;")
       nil)) ; (prevent "return debugger;" in compiled javascript)

(defmacro log-with-msg
  ([message x]
   `(let [x# ~x]
      (println (str ~message ": " '~x  " => "x#))
      x#)))

(defmacro log [& args]
  (assert false "the macro `log` is allowed only inside `deflog`"))

(defmacro defprint "thank you Herwig Hochleitner! https://groups.google.com/forum/#!topic/clojurescript/-iVx1UQRNSE" [func-name args & body]
  `(defn ~func-name [~'& args#]
     (println "args: " args#)
     (let [~args args#]
       ~@body)))


(defmacro deflog [& definition]
  (let [full-name (str (:name (cljs.analyzer/resolve-var &env (first definition))))];TODO support clj #?(:clj (resolve (first definition)))
    (if (vector? (second definition))
      (let [[func-name args & body] definition
            body-new (collections/my-replace {'log (list 'log-with-msg full-name)} body)]
        `(defn ~func-name ~args
           ~@body-new)))))

(defmacro deftrack [& definition]
  (let [full-name (str (:name (cljs.analyzer/resolve-var &env (first definition))))]
    (if (vector? (second definition))
      (let [[func-name args & body] definition]
        `(defn ~func-name [~'& args#]
           (assert (record-function-call ~full-name args#) (function-call-err-msg ~full-name args#))
           (let [~args args#]
             ~@body)))

      (let [[func-name & definitions] definition]
        `(defn ~func-name ~@(map
                              (fn [[args & body]]
                                `(~args
                                   (assert false "deftrack macro doesn't handle (yet) multi-arity functions")
                                   ~@body))
                              definitions))))))


;https://nvbn.github.io/2014/11/05/protocols-for-testing/ in the comments - same implementation as with-redefs from clojurescript, except that `~@(map bind-value binds)` is inside the `try` block
(defmacro with-redefs-safe
  [bindings & body]
  (let [names (take-nth 2 bindings)
        vals (take-nth 2 (drop 1 bindings))
        current-vals (map #(list 'identity %) names)
        tempnames (map (comp gensym name) names)
        binds (map vector names vals)
        resets (reverse (map vector names tempnames))
        bind-value (fn [[k v]] (list 'set! k v))]
    `(let [~@(interleave tempnames current-vals)]
       (try
         ~@(map bind-value binds)
         ~@body
         (finally
           ~@(map bind-value resets))))))


(defmacro my-with-redefs
  "like with-redefs but supports variables that contain a `dot` e.g. js/console.log"
    [bindings & body]
    (let [names (take-nth 2 bindings)
                  vals (take-nth 2 (drop 1 bindings))
                  tempnames (map (comp gensym #(clojure.string/replace  % #"\." "_") name) names)
                  binds (map vector names vals)
                  resets (reverse (map vector names tempnames))
                  bind-value (fn [[k v]] (list 'set! k v))]
          `(let [~@(interleave tempnames names)]
                    ~@(map bind-value binds)
                    (try
                                  ~@body
                               (finally
                                                   ~@(map bind-value resets))))))
