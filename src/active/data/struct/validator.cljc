(ns active.data.struct.validator)

(defprotocol IMapValidator
  "Note: Both methods are called on every construction and change;
  usually only one of them needs to be implemented, resp. the other as
  `(constantly nil)`."
  
  (-validate-field! [this changed-key new-value]
    "Throws an exception if the new value is not acceptable for the given key.")
  (-validate-map! [this m]
    "Throws an exception if an invariant is broken in the given map."
    ;; Note: m can be any kind of map; not guaranteed to be a
    ;; struct-map.
    ))

(defn validate-single! [t key value]
  (-validate-field! t key value))

(defn validate-map-only! [t m]
  (-validate-map! t m))

(defn validate! [t m changed-keys changed-values]
  (assert (map? m))
  (dorun (map (partial -validate-field! t)
              changed-keys
              changed-values))
  (-validate-map! t m))

(defn field-validators
  "Returns a map validator that checks some of the fields individually."
  [keys-fns-map]
  (let [lookup keys-fns-map]
    (reify IMapValidator
      (-validate-map! [this m] nil)
      (-validate-field! [this changed-key new-value]
        (when-let [f (lookup changed-key)]
          (f new-value))))))

(defn field-assertions
  "Returns a map validator that asserts that the given predicates hold for the given keys."
  [keys-predicates-map]
  (field-validators
   (into {} (map (fn [[k pred]]
                   (fn [v]
                     (assert (pred v) k)))
                 keys-predicates-map))))

(defn conditionally
  "Returns a validator that passes validation on to the given validator, if `(condition)` is true, and does nothing otherwise."
  [validator condition]
  (assert (satisfies? IMapValidator validator) validator)
  ;; FIXME: have condition be called only one time during one create/update.
  (reify IMapValidator
    (-validate-map! [this m]
      (when (condition)
        (-validate-map! validator m)))
    (-validate-field! [this changed-key new-value]
      (when (condition)
        (-validate-field! validator changed-key new-value)))))
