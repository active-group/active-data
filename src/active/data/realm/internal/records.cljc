(ns ^:no-doc active.data.realm.internal.records
  (:refer-clojure :exclude [keyword symbol char boolean seq compile record? delay delayed?
                            contains? uuid])
  (:require
   #?(:clj [clojure.core :as core]
      :cljs [cljs.core :as core])
   #?(:clj [active.data.raw-record :refer [def-record is-a?]]
      :cljs [active.data.raw-record :refer [is-a?] :refer-macros [def-record]])
   [active.data.realm.internal.record-meta :as realm-record-meta]))

(def-record realm
  [^{:doc "Description string for realm."}
   description
   ^{:doc "Shallow predicate for values of the realm."}
   predicate
   ^{:doc "Metadata for the realm, for additional semantically relevant information."}
   metadata])

(def-record ^{:doc "Builtin scalar realm."}
  builtin-scalar-realm
  :extends realm
  [builtin-scalar-realm-id])

(def-record ^{:doc "realm only defined through a predicate."}
  from-predicate-realm
  :extends realm
  [])

(def-record ^{:doc "realm of optional values."}
  optional-realm
  :extends realm
  [optional-realm-realm])

(def-record ^{:doc "realm for integer ranges."}
  integer-from-to-realm
  :extends realm
  ;; either can be nil
  [integer-from-to-realm-from integer-from-to-realm-to])

(def-record  ^{:doc "realm for real ranges."}
  real-range-realm
  :extends realm
  ; clusive is either :in or :ex
  [real-range-realm-clusive-left
   real-range-realm-left ; may be nil
   real-range-realm-right
   real-range-realm-clusive-right])

(def-record ^{:doc "realm for unions."}
  union-realm
  :extends realm
  ;; the shallow predicates of these should identify pairwise disjoint sets
  [union-realm-realms])

(def-record^{:doc "realm for enumerations."}
  enum-realm
  :extends realm
  [enum-realm-values]) ; set

(def-record ^{:doc "realm for intersections."}
  intersection-realm
  :extends realm
  ;; the shallow predicates of these should identify pairwise disjoint sets
  [intersection-realm-realms])

(def-record ^{:doc "realm for sequences."}
  sequence-of-realm
  :extends realm
  [sequence-of-realm-realm])

(def-record ^{:doc "realm for sets."}
  set-of-realm
  :extends realm
  [set-of-realm-realm])

;; keys you can live out just have optional realms

(def-record ^{:doc "realm for maps with certain constant keys."}
  map-with-keys-realm
  :extends realm
  [map-with-keys-realm-map])

(def-record ^{:doc "realm for maps with realms for keys and values respectively."}
  map-of-realm
  :extends realm
  [map-of-realm-key-realm map-of-realm-value-realm])

(def-record ^{:doc "realm for maps with a specific key, distinguishing it from other maps."}
  map-with-tag-realm
  :extends realm
  [map-with-tag-realm-key
   map-with-tag-realm-value])

(def-record ^{:doc "realm for tuples, i.e. sequences with a fixed number of elements, each of which has a realm."}
  tuple-realm
  :extends realm
  [tuple-realm-realms])

(def-record ^{:doc "Description of the field of a record."}
  record-realm-field
  [record-realm-field-name record-realm-field-realm record-realm-field-getter])

(def-record ^{:doc "realm for records."}
  record-realm
  :extends realm
  [record-realm-name
   record-realm-constructor
   record-realm-fields])

(def-record ^{:doc "Function case."}
  function-case
  [function-case-positional-argument-realms ; seq of realms
   function-case-optional-arguments-realm ; nil or sequence-of realm or map-with-keys realm or tuple realm
   function-case-return-realm])

(def-record ^{:doc "realm for function with multiple cases."}
  function-realm
  :extends realm
  [function-realm-cases])

(def-record ^{:doc "Lazy realm."}
  delayed-realm
  :extends realm
  [delayed-realm-delay])

; for documenting polymorphism
(def-record ^{:doc "Named realm."}
  named-realm
  :extends realm
  [named-realm-name ; keyword
   named-realm-realm])





