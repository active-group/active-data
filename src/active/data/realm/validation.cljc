(ns active.data.realm.validation
  (:require [schema.core :as schema]
            [active.data.realm.schema :as realm-schema]))

(defmacro checking
  "Execute body with input and output schema validation turned on for
  all realm-attach/defn and realm-attach/fn instances globally (across
  all threads). After all forms have been executed, resets function
  validation to its previously set value. Not concurrency-safe."
  [& body]
  `(schema/with-fn-validation ~@body))

(def set-checking! schema/set-fn-validation!)

(def checking? schema/fn-validation?)

(defn validator
  [realm]
  (let [schema (realm-schema/schema realm)]
    (fn [thing]
      (schema/validate schema thing))))
