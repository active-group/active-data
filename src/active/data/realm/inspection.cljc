(ns active.data.realm.inspection
  (:refer-clojure :exclude [record? char keyword symbol boolean uuid])
  (:require [active.data.realm.internal.records :as realm-records]
            [active.data.realm :as realm]
            #?(:clj [active.data.raw-record :refer [def-record is-a?]]
               :cljs [active.data.raw-record :refer [is-a?] :refer-macros [def-record]])
            #?(:clj [active.data.internal.export :refer [re-export]]
               :cljs [active.data.internal.export :refer-macros [re-export]])))

(defn realm?
  [thing]
  (is-a? realm-records/realm thing))

(re-export
 realm-records/description
 realm-records/predicate
 realm-records/metadata)

(re-export
  realm-records/builtin-scalar-realm
  realm-records/builtin-scalar-realm-id)

(defn builtin-scalar?
  [thing]
  (is-a? builtin-scalar-realm thing))

(re-export realm-records/from-predicate-realm)

(defn from-predicate?
  [thing]
  (is-a? from-predicate-realm thing))

(re-export
  realm-records/optional-realm
  realm-records/optional-realm-realm)

(defn optional?
  [thing]
  (is-a? optional-realm thing))

(re-export
  realm-records/integer-from-to-realm
  realm-records/integer-from-to-realm-from
  realm-records/integer-from-to-realm-to)

(defn real-range?
  [thing]
  (is-a? real-range? thing))

(re-export
  realm-records/union-realm
  realm-records/union-realm-realms)

(defn union?
  [thing]
  (is-a? union-realm thing))

(re-export
  realm-records/enum-realm
  realm-records/enum-realm-values)

(defn enum?
  [thing]
  (is-a? enum-realm thing))

(re-export
  realm-records/intersection-realm
  realm-records/intersection-realm-realms)

(defn intersection?
  [thing]
  (is-a? intersection-realm thing))

(re-export
  realm-records/sequence-of-realm
  realm-records/sequence-of-realm-realm)

(defn sequence-of?
  [thing]
  (is-a? sequence-of-realm thing))

(re-export
  realm-records/set-of-realm
  realm-records/set-of-realm-realm)

(defn set-of?
  [thing]
  (is-a? set-of-realm thing))

(re-export
  realm-records/map-with-keys-realm
  realm-records/map-with-keys-realm-map)

(defn map-with-keys?
  [thing]
  (is-a? map-with-keys-realm thing))

(re-export
  realm-records/map-of-realm
  realm-records/map-of-realm-key-realm
  realm-records/map-of-realm-value-realm)

(defn map-of?
  [thing]
  (is-a? map-of-realm thing))

(re-export
  realm-records/map-with-tag-realm
  realm-records/map-with-tag-realm-key
  realm-records/map-with-tag-realm-value)

(defn map-with-tag?
  [thing]
  (is-a? map-with-tag-realm thing))

(re-export
  realm-records/tuple-realm
  realm-records/tuple-realm-realms)

(defn tuple?
  [thing]
  (is-a? tuple-realm thing))

(re-export
  realm-records/record-realm-field
  realm-records/record-realm-field-name
  realm-records/record-realm-field-realm
  realm-records/record-realm-field-getter

  realm-records/record-realm
  realm-records/record-realm-name
  realm-records/record-realm-constructor
  realm-records/record-realm-fields)

(defn record?
  [thing]
  (is-a? record-realm thing))

(re-export
  realm-records/function-case
  realm-records/function-case-positional-argument-realms
  realm-records/function-case-optional-arguments-realm
  realm-records/function-case-return-realm

  realm-records/function-realm
  realm-records/function-realm-cases)

(defn function?
  [thing]
  (is-a? function-realm thing))

(re-export
 realm-records/delayed-realm
 realm-records/delayed-realm-delay)

(defn delayed?
  [thing]
  (is-a? delayed-realm thing))

(re-export
  realm-records/named-realm
  realm-records/named-realm-name
  realm-records/named-realm-realm)

(defn named?
  [thing]
  (is-a? named-realm thing))

; questionable
(def builtin-scalar (realm/record->record-realm realm-records/builtin-scalar-realm))

#?(:clj (def rational (realm/enum realm/rational)))
(def number (realm/enum realm/number))
(def char (realm/enum realm/char))
(def keyword (realm/enum realm/keyword))
(def symbol (realm/enum realm/symbol))
(def string (realm/enum realm/string))
(def boolean (realm/enum realm/boolean))
(def uuid (realm/enum realm/uuid))
(def any (realm/enum realm/any))

(def from-predicate (realm/record->record-realm realm-records/from-predicate-realm))
(def optional (realm/record->record-realm realm-records/optional-realm))
(def integer-from-to (realm/record->record-realm realm-records/integer-from-to-realm))
(def real-range (realm/record->record-realm realm-records/real-range-realm))
(def union  (realm/record->record-realm realm-records/union-realm))
(def intersection  (realm/record->record-realm realm-records/intersection-realm))
(def enum  (realm/record->record-realm realm-records/enum-realm))
(def sequence-of  (realm/record->record-realm realm-records/sequence-of-realm))
(def set-of  (realm/record->record-realm realm-records/set-of-realm))
(def map-with-keys  (realm/record->record-realm realm-records/map-with-keys-realm))
(def map-with-tag  (realm/record->record-realm realm-records/map-with-tag-realm))
(def map-of  (realm/record->record-realm realm-records/map-of-realm))
(def tuple  (realm/record->record-realm realm-records/tuple-realm))
(def record  (realm/record->record-realm realm-records/record-realm))
(def function  (realm/record->record-realm realm-records/function-realm))
(def delayed  (realm/record->record-realm realm-records/delayed-realm))
(def named  (realm/record->record-realm realm-records/named-realm))

(def realm
  (realm/union
   #?(:clj rational)
   number
   keyword
   symbol
   string
   boolean
   uuid
   any
   from-predicate
   optional
   integer-from-to
   real-range
   union
   intersection
   enum
   sequence-of
   set-of
   map-with-keys
   map-of
   map-with-tag
   tuple
   record
   function
   delayed
   named))
   
