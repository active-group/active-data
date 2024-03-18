(ns active.data.realm.realms
  (:refer-clojure :exclude [keyword symbol boolean char uuid])
  (:require [active.data.realm :as realm]
            [active.data.realm.internal.records :as realm-records]))

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
   
