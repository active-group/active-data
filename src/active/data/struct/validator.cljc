(ns active.data.struct.validator)

(defprotocol IMapValidator
  "Note: Both methods are called on every construction and change;
  usually only one of them needs to be implemented, resp. the other as
  `(constantly nil)`."
  
  (-validate-field! [this changed-key new-value]
    "Throws an exception if the new value is not acceptable for the given key.")
  (-validate-map! [this m]
    "Throws an exception if an invariant is broken in the given map. Note
     that in general, unknown keys in the map should be ignored by the
     validator."
    ;; Note: m can be any kind of map; not guaranteed to be a
    ;; struct-map.  Note: when used in [[satisfies?]] resp. [[valid?]]
    ;; or extending struct, the map may contain more keys; validators
    ;; should only check the fields they know of. (maybe add 'valid?'
    ;; to the protocol; also removes the need for try-catch.)
    ))

(defn validate-single! [t key value]
  (-validate-field! t key value))

(defn validate-map-only! [t m]
  ;; OPT: have a variant without 'changed key'; might be counter-productive to pass just all of them here.
  (-validate-map! t m #_(keys m)))

(defn validate! [t m changed-keys changed-values]
  (assert (map? m))
  (dorun (map (partial -validate-field! t)
              changed-keys
              changed-values))
  (-validate-map! t m #_changed-keys))

(defn valid? [t m]
  ;; Note: a pity that we have to try-catch here, but the advantage of
  ;; having the validate-fns throw, is that the stack traces for errors
  ;; will directly point at the validator impl.
  (try (validate! t m (keys m) (vals m))
       true
       #?(:clj (catch Throwable e
                 false)
          :cljs (catch :default e
                  false))))

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

;; TODO: (when ^boolean js/goog.DEBUG)?, although asserts are already
;; removable in CLJS, and there seems to be no standard way in CLJ
;; (https://ask.clojure.org/index.php/1529/debug-builds)

(defn conditionally
  "Returns a validator that passes validation on to the given validator, if `@var` is true, and does nothing otherwise."
  [var validator]
  (assert (satisfies? IMapValidator validator))
  (assert (satisfies? clojure.lang.IDeref validator))
  (reify IMapValidator
    (-validate-map! [this m]
      (when @var
        (-validate-map! validator m)))
    (-validate-field! [this changed-key new-value]
      (when @var
        (-validate-field! changed-key changed-key new-value)))))
