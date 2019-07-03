(ns gadjett.utils)

(defn adv-sort-by [key-ord-pairs col]
  (let [key-fns (take-nth 2 key-ord-pairs)
        key-to-ord (apply hash-map key-ord-pairs)
        key-to-idx (zipmap key-fns (range (count key-fns)))]
    (sort-by
      (apply juxt key-fns)
      (fn [x y]
        (let [juxt-manipulator #(case (key-to-ord %3)
                                  :ascending (%1 (key-to-idx %3))
                                  :descending (%2 (key-to-idx %3)))
              x-juxt (mapv (partial juxt-manipulator x y) key-fns)
              y-juxt (mapv (partial juxt-manipulator y x) key-fns)]
          (compare x-juxt y-juxt)))
      col)))
