(ns active.data.realm.schema
  (:require [active.data.realm :as realm]
            [schema.core :as schema]))

(defn schema
  [realm]
  (cond
    (realm/builtin-scalar? realm)
    (case (realm/builtin-scalar-realm-id realm)
      (:float) float
      (:double) double
      (:int) schema/Int
      (:bigdec) java.math.BigDecimal
      (:keyword) schema/Keyword
      (:symbol) schema/Symbol
      (:string) schema/Str
      (:any) schema/Any)

    (realm/predicate? realm)
    (schema/pred (realm/predicate-realm-predicate realm))

    (realm/optional? realm)
    (schema/maybe (schema (realm/optional-realm-realm realm)))))
    
