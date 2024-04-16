(ns ^{:doc "Validate value realm membership.

Currently implemented via Schema."}
 active.data.realm.validation
  (:require [schema.core :as schema]
            [active.data.realm.schema :as realm-schema]))

(defmacro checking
  "Execute body with input and output schema validation turned on for
  all realm-attach/defn and realm-attach/fn instances globally (across
  all threads). After all forms have been executed, resets function
  validation to its previously set value. Not concurrency-safe."
  [& body]
  `(schema/with-fn-validation ~@body))

(def ^{:doc "Set whether validation takes place."}
  set-checking! schema/set-fn-validation!)

(def ^{:doc "Return a boolean that tells whether validation is taking place."} checking? schema/fn-validation?)

(defn validator
  [realm]
  "Return a validation function for a given realm.

This function accepts a value and will raise an exception
if validation fails."
  (let [schema (realm-schema/schema realm)]
    (fn [thing]
      (schema/validate schema thing))))
