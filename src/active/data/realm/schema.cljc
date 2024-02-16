(ns active.data.realm.schema
  (:require [active.data.realm :as realm #?@(:cljs [:include-macros true])]
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
      #?@(:clj [(:bigdec) java.math.BigDecimal])
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

    map-of?
    {(schema (realm/map-of-realm-key-realm realm))
     (schema (realm/map-of-realm-value-realm realm))}

    set-of?
    #{(schema (realm/set-of-realm-realm realm))}
    
    integer-from-to?
    (let [from (realm/integer-from-to-realm-from realm)
          to (realm/integer-from-to-realm-to realm)]
      (schema/constrained schema/Int
                          (fn [n]
                            (<= from n to))))

    union?
    (loop [realms (realm/union-realm-realms realm)
           args (transient [])]
      (if (empty? realms)
        (apply schema/conditional (persistent! args))
        (recur (rest realms)
               (conj! (conj! args (realm/shallow-predicate (first realms)))
                      (schema (first realms))))))

    sequence-of?
    [(schema (realm/sequence-of-realm-realm realm))]

    map-with-keys?
    (into {}
          (map (fn [[key realm]]
                 (if (realm/optional? realm)
                   [(schema/optional-key key)
                    (schema (realm/optional-realm-realm realm))]
                   [(schema/required-key key)
                    (schema realm)]))
               (realm/map-with-keys-realm-map realm)))
    
    :else
    (throw (ex-info (str "unhandled realm case: " (realm/description realm)) {:active.data.realm/realm realm}))
    
  ))
    
