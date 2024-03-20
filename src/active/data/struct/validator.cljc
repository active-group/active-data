(ns active.data.struct.validator
  (:require [active.data.struct.internal.validator :as internal]))

(defn field-validators
  "Returns a map validator that checks some of the fields individually."
  [keys-fns-map]
  (let [lookup keys-fns-map]
    (reify internal/IMapValidator
      (-validate-map! [this m] nil)
      (-validate-field! [this changed-key new-value]
        (when-let [f (lookup changed-key)]
          (f new-value))))))

(defn field-assertions
  "Returns a map validator that asserts that the given predicates hold for the given keys."
  [keys-predicates-map]
  (field-validators
   (into {} (map (fn [[k pred]]
                   [k (fn [v]
                        (assert (pred v) k))])
                 keys-predicates-map))))

(defn conditionally
  "Returns a validator that passes validation on to the given validator, if `(condition)` is true, and does nothing otherwise."
  [validator condition]
  (if (satisfies? internal/IDeferredMapValidator validator)
    ;; resolve deferred->deferred
    (reify internal/IDeferredMapValidator
      (-get-validator! [this]
        (when (condition)
          (internal/-get-validator! validator))))
    (do
      (assert (satisfies? internal/IMapValidator validator) validator)
      (reify internal/IDeferredMapValidator
        (-get-validator! [this]
          (when (condition)
            validator))))))
