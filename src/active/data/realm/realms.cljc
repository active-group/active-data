(ns active.data.realm.realms
  (:refer-clojure :exclude [keyword symbol boolean])
  (:require [active.data.realm :as realm]))

; questionable
(def builtin-scalar (realm/record->record-realm realm/builtin-scalar-realm))

(def natural (realm/enum realm/natural))
(def integer (realm/enum realm/integer))
#?(:clj (def rational (realm/enum realm/rational)))
(def real (realm/enum realm/real))
(def number (realm/enum realm/number))
(def keyword (realm/enum realm/keyword))
(def symbol (realm/enum realm/symbol))
(def string (realm/enum realm/string))
(def boolean (realm/enum realm/boolean))
(def any (realm/enum realm/any))

(def from-predicate (realm/record->record-realm realm/from-predicate-realm))
(def optional (realm/record->record-realm realm/optional-realm))
(def integer-from-to (realm/record->record-realm realm/integer-from-to-realm))
(def union  (realm/record->record-realm realm/union-realm))
(def intersection  (realm/record->record-realm realm/intersection-realm))
(def enum  (realm/record->record-realm realm/enum-realm))
(def sequence-of  (realm/record->record-realm realm/sequence-of-realm))
(def set-of  (realm/record->record-realm realm/set-of-realm))
(def map-with-keys  (realm/record->record-realm realm/map-with-keys-realm))
(def map-of  (realm/record->record-realm realm/map-of-realm))
(def tuple  (realm/record->record-realm realm/tuple-realm))
(def record  (realm/record->record-realm realm/record-realm))
(def function  (realm/record->record-realm realm/function-realm))
(def delayed  (realm/record->record-realm realm/delayed-realm))
(def named  (realm/record->record-realm realm/named-realm))
(def restricted  (realm/record->record-realm realm/restricted-realm))

(def realm
  (realm/union
   natural
   integer
   rational
   real
   number
   keyword
   symbol
   string
   boolean
   any
   from-predicate
   optional
   integer-from-to
   union
   intersection
   enum
   sequence-of
   set-of
   map-with-keys
   map-of
   tuple
   record
   function
   delayed
   named
   restricted))
   