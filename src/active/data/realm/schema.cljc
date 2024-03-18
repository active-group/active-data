(ns active.data.realm.schema
  (:require [active.data.realm.inspection :as realm-inspection]
            [active.data.realm.dispatch :as realm-dispatch #?@(:cljs [:include-macros true])]
            [active.data.realm.realms :as realms]
            [schema.core :as schema]
            [schema.utils :as schema-utils]
            [schema.spec.core :as schema-spec :include-macros true]
            #?(:clj [schema.macros :as schema-macros]))
  #?(:cljs (:require-macros [schema.macros :as schema-macros])))

;; essentially a copy of Schema's Both, which is deprecated
(schema-macros/defrecord-schema ^:private Intersection [schemas]
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
  (let [returns (into #{} (map realm-inspection/function-case-return-realm cases))]
    (if (= (count returns) 1)
      (schema (first returns))
      schema/Any))) ; cop out

(defn- fn-input-schema
  [case]
  (let [positional-schemas
        (vec
         (map-indexed (fn [index realm]
                        (schema/one (schema realm) (str "arg" index)))
                      (realm-inspection/function-case-positional-argument-realms case)))
        optional (realm-inspection/function-case-optional-arguments-realm case)]
    (cond
      (nil? optional)
      positional-schemas

      (vector? optional)
      (vec (concat positional-schemas
                   (mapv schema optional)))

      :else
      positional-schemas))) ; FIXME: knowing that schema doesn't handle this and won't check this

(def ^:private generic-function-schema (schema/pred fn? "schema for unknown function"))

(defn schema
  [realm]
  (realm-dispatch/union-case
   realms/realm realm

   #?@(:clj [realms/rational (schema/pred rational?)])
   realms/number schema/Num

   realms/char (schema/pred char?)

   realms/boolean schema/Bool
   realms/keyword schema/Keyword
   realms/symbol schema/Symbol
   realms/string schema/Str
   realms/uuid schema/Uuid
   realms/any schema/Any

   realms/from-predicate
   (schema/pred (realm-inspection/predicate realm)
                (realm-inspection/description realm))

   realms/optional
   (schema/maybe (schema (realm-inspection/optional-realm-realm realm)))
   
   realms/tuple
   (vec (map-indexed (fn [index realm]
                       (schema/one (schema realm) (str index)))
                     (realm-inspection/tuple-realm-realms realm)))

   realms/map-of
   {(schema (realm-inspection/map-of-realm-key-realm realm))
    (schema (realm-inspection/map-of-realm-value-realm realm))}
   
   realms/set-of
   #{(schema (realm-inspection/set-of-realm-realm realm))}
   
   realms/integer-from-to
   (let [from (realm-inspection/integer-from-to-realm-from realm)
         to (realm-inspection/integer-from-to-realm-to realm)]
     (if (and (nil? from) (nil? to))
       schema/Int
       (schema/constrained schema/Int
                           (realm-inspection/predicate realm))))
   
   realms/real-range
   (schema/constrained schema/Num
                       (realm-inspection/predicate realm))
   

   realms/union
   (loop [realms (realm-inspection/union-realm-realms realm)
          args (transient [])]
     (if (empty? realms)
       (apply schema/conditional (persistent! args))
       (recur (rest realms)
              (conj! (conj! args (realm-inspection/predicate (first realms)))
                     (schema (first realms))))))
   
   realms/intersection
   (Intersection. (map schema (realm-inspection/intersection-realm-realms realm)))
   
   realms/enum
   (apply schema/enum (realm-inspection/enum-realm-values realm))

   realms/sequence-of
   [(schema (realm-inspection/sequence-of-realm-realm realm))]

   realms/map-with-keys
   (into {}
         (map (fn [[key realm]]
                (if (realm-inspection/optional? realm)
                  [(schema/optional-key key)
                   (schema (realm-inspection/optional-realm-realm realm))]
                  [(schema/required-key key)
                   (schema realm)]))
              (realm-inspection/map-with-keys-realm-map realm)))

   realms/map-with-tag
   {(schema/required-key (realm-inspection/map-with-tag-realm-key realm))
    (schema/eq (realm-inspection/map-with-tag-realm-value realm))
    schema/Any schema/Any}

   realms/record
   (schema/pred (realm-inspection/predicate realm)
                (str (realm-inspection/record-realm-name realm) " record"))

   realms/named
   (schema/schema-with-name (schema (realm-inspection/named-realm-realm realm))
                            (realm-inspection/named-realm-name realm))

   realms/delayed
   (schema/recursive (delay (schema (realm-inspection/delayed-realm-delay realm))))

   realms/function
   (let [cases (realm-inspection/function-realm-cases realm)]
     (try
       (schema/make-fn-schema (fn-output-schema cases)
                              (map fn-input-schema cases))
       (catch #?(:clj RuntimeException :cljs js/Error) e
         generic-function-schema)))))
