(ns active.data.realm.schema
  (:require [active.data.realm :as realm #?@(:cljs [:include-macros true])]
            [schema.core :as schema]
            [schema.utils :as schema-utils]
            [schema.spec.core :as schema-spec :include-macros true]
            #?(:clj [schema.macros :as schema-macros]))
  #?(:cljs (:require-macros [schema.macros :as schema-macros])))

;; essentially a copy of Schema's Both, which is deprecated
(schema-macros/defrecord-schema Intersection [schemas]
  schema/Schema
  (spec [this] this)
  (explain [this] (cons 'intersection (map schema/explain schemas)))
  schema/HasPrecondition
  (precondition [this]
    (apply every-pred (map (comp schema/precondition schema/spec) schemas)))
  schema-spec/CoreSpec
  (subschemas [this] schemas)
  (checker [this params]
    (reduce
     (fn [f t]
       (fn [x]
         (let [tx (t x)]
           (if (schema-utils/error? tx)
             tx
             (f (or tx x))))))
     (map #(schema-spec/sub-checker {:schema %} params) (reverse schemas)))))

(declare schema)

; schema allows only one output schema for a fn
(defn- fn-output-schema
  [cases]
  (let [returns (into #{} (map realm/function-case-return-realm cases))]
    (if (= (count returns) 1)
      (schema (first returns))
      realm/any))) ; cop out

(defn- fn-input-schema
  [case]
  (let [positional-schemas
        (vec
         (map-indexed (fn [index realm]
                        (schema/one (schema realm) (str "arg" index)))
                      (realm/function-case-positional-argument-realms case)))
        optional (realm/function-case-optional-arguments-realm case)]
    (cond
      (nil? optional)
      positional-schemas

      (vector? optional)
      (vec (concat positional-schemas
                   (mapv schema optional)))

      :else
      positional-schemas))) ; FIXME: knowing that schema doesn't handle this and won't check this

(def generic-function-schema (schema/pred fn? "schema for unknown function"))
(def natural-schema (schema/constrained schema/Int (fn [n] (>= n 0)) "natural"))

(defn schema
  [realm]
  (realm/dispatch
      realm
    builtin-scalar?
    (case (realm/builtin-scalar-realm-id realm)
      (:natural) natural-schema
      (:integer) schema/Int
      #?@(:clj [:rational (schema/pred rational?)])
      (:real) schema/Num
      (:number) schema/Num

      (:boolean) boolean
      (:keyword) schema/Keyword
      (:symbol) schema/Symbol
      (:string) schema/Str
      (:any) schema/Any)

    predicate?
    (schema/pred (realm/predicate-realm-predicate realm)
                 (realm/description realm))

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

    intersection?
    (Intersection. (map schema (realm/intersection-realm-realms realm)))
    
    enum?
    (apply schema/enum (realm/enum-realm-values realm))

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

    restricted?
    (schema/constrained (schema (realm/restricted-realm-realm realm))
                        (realm/restricted-realm-predicate realm)
                        (realm/description realm))

    record?
    (schema/pred (realm/record-realm-predicate realm)
                 (str (realm/record-realm-name realm) " record"))

    named?
    (schema/schema-with-name (schema (realm/named-realm-realm realm))
                             (realm/named-realm-name realm))

    delayed?
    (schema/recursive (delay (schema (realm/delayed-realm-delay realm))))

    function?
    (let [cases (realm/function-realm-cases realm)]
      (try
        (schema/make-fn-schema (fn-output-schema cases)
                               (map fn-input-schema cases))
        (catch #?(:clj RuntimeException :cljs js/Error) e
            generic-function-schema)))))
