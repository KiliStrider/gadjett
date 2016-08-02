(ns gadjett.macros)


#_(defn symbol-several
  "returns a symbol with the concatenation of the str values of the args"
  [& x]
  (symbol (apply str x)))

#_(defmacro disp [& forms]
  (cons
    `symbol-several
    (for [form forms]
      `(str '~form " =>> " ~form "\n"))))
    
(defmacro disp [& forms]
  (cons `str (for [form forms]
               `(str (pr-str '~form) " => " (pr-str ~form)))))
