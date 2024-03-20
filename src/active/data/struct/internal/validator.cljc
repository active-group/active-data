(ns active.data.struct.internal.validator)

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

(defprotocol IDeferredMapValidator
  (-get-validator! [this] "Return the the actual IMapValidator, or nil. Called once for each create/update of a map."))

(defn resolve! [t]
  (if (satisfies? IDeferredMapValidator t)
    (-get-validator! t)
    t))

(defn validate-single! [t key value]
  (-validate-field! t key value))

(defn validate-map-only! [t m]
  (-validate-map! t m))

(defn validate!
  ([t m]
   (assert (map? m))
   (validate! t m (keys m) (vals m)))
  ([t m changed-keys changed-values]
   (dorun (map (partial -validate-field! t)
               changed-keys
               changed-values))
   (-validate-map! t m)))

