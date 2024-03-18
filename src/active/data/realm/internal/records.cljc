(ns active.data.realm.internal.records
  (:refer-clojure :exclude [keyword symbol char boolean seq compile record? delay delayed?
                            contains? uuid])
  (:require
   #?(:clj [clojure.core :as core]
      :cljs [cljs.core :as core])
   #?(:clj [active.data.record :refer [def-record is-a?]]
      :cljs [active.data.record :refer [is-a?] :refer-macros [def-record]])
   [active.data.struct :as struct]
   [active.data.record :as record]
   [active.data.realm.realm-record-meta :as realm-record-meta]
   [clojure.set :as set]
   [clojure.string :as string]))

(def-record Realm [description predicate metadata])

(def-record ^{:doc "Builtin scalar realm."}
  builtin-scalar-realm
  :extends Realm
  [builtin-scalar-realm-id])

(def-record ^{:doc "Realm only defined through a predicate."}
  from-predicate-realm
  :extends Realm
  [])

(def-record ^{:doc "Realm of optional values."}
  optional-realm
  :extends Realm
  [optional-realm-realm])

(def-record ^{:doc "Realm for integer ranges."}
  integer-from-to-realm
  :extends Realm
  ;; either can be nil
  [integer-from-to-realm-from integer-from-to-realm-to])

(def-record  ^{:doc "Realm for real ranges."}
  real-range-realm
  :extends Realm
  ; clusive is either :in or :ex
  [real-range-realm-clusive-left
   real-range-realm-left ; may be nil
   real-range-realm-right
   real-range-realm-clusive-right])

(def-record ^{:doc "Realm for unions."}
  union-realm
  :extends Realm
  ;; the shallow predicates of these should identify pairwise disjoint sets
  [union-realm-realms])

(def-record^{:doc "Realm for enumerations."}
  enum-realm
  :extends Realm
  [enum-realm-values]) ; set

(def-record ^{:doc "Realm for intersections."}
  intersection-realm
  :extends Realm
  ;; the shallow predicates of these should identify pairwise disjoint sets
  [intersection-realm-realms])

(def-record ^{:doc "Realm for sequences."}
  sequence-of-realm
  :extends Realm
  [sequence-of-realm-realm])

(def-record ^{:doc "Realm for sets."}
  set-of-realm
  :extends Realm
  [set-of-realm-realm])

;; keys you can live out just have optional realms

(def-record ^{:doc "Realm for maps with certain constant keys."}
  map-with-keys-realm
  :extends Realm
  [map-with-keys-realm-map])

(def-record ^{:doc "Realm for maps with realms for keys and values respectively."}
  map-of-realm
  :extends Realm
  [map-of-realm-key-realm map-of-realm-value-realm])

(def-record ^{:doc "Realm for maps with a specific key, distinguishing it from other maps."}
  map-with-tag-realm
  :extends Realm
  [map-with-tag-realm-key
   map-with-tag-realm-value])

(def-record ^{:doc "Realm for tuples, i.e. sequences with a fixed number of elements, each of which has a realm."}
  tuple-realm
  :extends Realm
  [tuple-realm-realms])

(def-record ^{:doc "Description of the field of a record."}
  record-realm-field
  [record-realm-field-name record-realm-field-realm record-realm-field-getter])

(def-record ^{:doc "Realm for records."}
  record-realm
  :extends Realm
  [record-realm-name
   record-realm-constructor
   record-realm-fields])

(def-record ^{:doc "Function case."}
  function-case
  [function-case-positional-argument-realms ; seq of realms
   function-case-optional-arguments-realm ; nil or sequence-of realm or map-with-keys realm or tuple realm
   function-case-return-realm])

(def-record ^{:doc "Realm for function with multiple cases."}
  function-realm
  :extends Realm
  [function-realm-cases])

(def-record ^{:doc "Lazy realm."}
  delayed-realm
  :extends Realm
  [delayed-realm-delay])

; for documenting polymorphism
(def-record ^{:doc "Named realm."}
  named-realm
  :extends Realm
  [named-realm-name ; keyword
   named-realm-realm])





