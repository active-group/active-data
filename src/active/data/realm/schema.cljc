(ns active.data.realm.schema
  (:require [active.data.realm :as realm]
            [schema.core :as schema]))

(defn schema
  [realm]
  (realm/dispatch
      realm
    builtin-scalar?
    (case (realm/builtin-scalar-realm-id realm)
      (:float) float
      (:double) double
      (:int) schema/Int
      (:bigdec) java.math.BigDecimal
      (:keyword) schema/Keyword
      (:symbol) schema/Symbol
      (:string) schema/Str
      (:any) schema/Any)

    predicate?
    (schema/pred (realm/predicate-realm-predicate realm))

    optional?
    (schema/maybe (schema (realm/optional-realm-realm realm)))

    tuple?
    (vec (map-indexed (fn [index realm]
                        (schema/one (schema realm) (str index)))
                      (realm/tuple-realm-realms realm)))
    
    :else
    (throw (ex-info (str "unhandled realm case: " (realm/description realm)) {:active.data.realm/realm realm}))
    
  ))
    
