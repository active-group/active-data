(ns active.data.realm.inspection
  (:refer-clojure :exclude [record?])
  (:require [active.data.realm.internal.records :as realm-records]
            #?(:clj [active.data.record :refer [def-record is-a?]]
               :cljs [active.data.record :refer [is-a?] :refer-macros [def-record]])))

(defn realm?
  [thing]
  (is-a? realm-records/Realm thing))

(def description realm-records/description)
(def predicate realm-records/predicate)
(def metadata realm-records/metadata)

(def builtin-scalar-realm realm-records/builtin-scalar-realm)

(def builtin-scalar-realm-id realm-records/builtin-scalar-realm-id)

(defn builtin-scalar?
  [thing]
  (is-a? builtin-scalar-realm thing))

(def from-predicate-realm realm-records/from-predicate-realm)

(defn from-predicate?
  [thing]
  (is-a? from-predicate-realm thing))

(def optional-realm realm-records/optional-realm)
(def optional-realm-realm realm-records/optional-realm-realm)

(defn optional?
  [thing]
  (is-a? optional-realm thing))

(def integer-from-to-realm realm-records/integer-from-to-realm)
(def integer-from-to-realm-from realm-records/integer-from-to-realm-from)
(def integer-from-to-realm-to realm-records/integer-from-to-realm-to)

(defn real-range?
  [thing]
  (is-a? real-range? thing))

(def union-realm realm-records/union-realm)
(def union-realm-realms realm-records/union-realm-realms)

(defn union?
  [thing]
  (is-a? union-realm thing))

(def enum-realm realm-records/enum-realm)
(def enum-realm-values realm-records/enum-realm-values)

(defn enum?
  [thing]
  (is-a? enum-realm thing))

(def intersection-realm realm-records/intersection-realm)
(def intersection-realm-realms realm-records/intersection-realm-realms)

(defn intersection?
  [thing]
  (is-a? intersection-realm thing))

(def sequence-of-realm realm-records/sequence-of-realm)
(def sequence-of-realm-realm realm-records/sequence-of-realm-realm)

(defn sequence-of?
  [thing]
  (is-a? sequence-of-realm thing))

(def set-of-realm realm-records/set-of-realm)
(def set-of-realm-realm realm-records/set-of-realm-realm)

(defn set-of?
  [thing]
  (is-a? set-of-realm thing))

(def map-with-keys-realm realm-records/map-with-keys-realm)
(def map-with-keys-realm-map realm-records/map-with-keys-realm-map)

(defn map-with-keys?
  [thing]
  (is-a? map-with-keys-realm thing))

(def map-of-realm realm-records/map-of-realm)
(def map-of-realm-key-realm realm-records/map-of-realm-key-realm)
(def map-of-realm-value-realm realm-records/map-of-realm-value-realm)

(defn map-of?
  [thing]
  (is-a? map-of-realm thing))

(def map-with-tag-realm realm-records/map-with-tag-realm)
(def map-with-tag-realm-key realm-records/map-with-tag-realm-key)
(def map-with-tag-realm-value realm-records/map-with-tag-realm-value)

(defn map-with-tag?
  [thing]
  (is-a? map-with-tag-realm thing))

(def tuple-realm realm-records/tuple-realm)
(def tuple-realm-realms realm-records/tuple-realm-realms)

(defn tuple?
  [thing]
  (is-a? tuple-realm thing))

(def record-realm-field realm-records/record-realm-field)
(def record-realm-field-name realm-records/record-realm-field-name)
(def record-realm-field-realm realm-records/record-realm-field-realm)
(def record-realm-field-getter realm-records/record-realm-field-getter)

(def record-realm realm-records/record-realm)
(def record-realm-name realm-records/record-realm-name)
(def record-realm-constructor realm-records/record-realm-constructor)
(def record-realm-fields realm-records/record-realm-fields)

(defn record?
  [thing]
  (is-a? record-realm thing))

(def function-case realm-records/function-case)
(def function-case-positional-argument-realms realm-records/function-case-positional-argument-realms)
(def function-case-optional-arguments-realm realm-records/function-case-optional-arguments-realm)
(def function-case-return-realm realm-records/function-case-return-realm)

(def function-realm realm-records/function-realm)
(def function-realm-cases realm-records/function-realm-cases)

(defn function?
  [thing]
  (is-a? function-realm thing))

(def delayed-realm realm-records/delayed-realm)
(def delayed-realm-delay realm-records/delayed-realm-delay)

(defn delayed?
  [thing]
  (is-a? delayed-realm thing))

(def named-realm realm-records/named-realm)
(def named-realm-name realm-records/named-realm-name)
(def named-realm-realm realm-records/named-realm-realm)

(defn named?
  [thing]
  (is-a? named-realm thing))