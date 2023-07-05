(ns active.data.realm
  #?@
  (:clj
   [(:refer-clojure :exclude [int bigdec float double keyword symbol seq compile record?
                              struct-map instance? satisfies? set-validator!])
    (:require
     [clojure.core :as core]
     [active.data.struct :refer :all]
     [clojure.set :as set]
     [clojure.string :as string])
    (:import java.net.URL)
    ]
   :cljs
   [(:require [clojure.set :as set]
              [clojure.string :as string]
              active.clojure.cljs.record)
    (:require-macros
     [active.clojure.cljs.record :refer [define-record-type]])]))

(def-struct Realm [description metadata])

(defn realm?
  [thing]
  (satisfies? Realm thing))

;; builtin-scalar-realm
;; predicate-realm
;; optional-realm
;; integer-from-to-realm
;; mixed-realm
;; intersection-realm
;; enum-realm
;; sequence-of-realm
;; array-of-realm
;; set-of-realm
;; map-with-keys-realm
;; map-of-realm
;; tuple-of-realm
;; record-realm
;; function realm: optional arguments ... keywords?
;; restricted-realm

;; nonempty-string-realm
;; max-length-string-realm

(def-struct ^{:doc "Builtin scalar realm."}
  builtin-scalar-realm
  :extends Realm
  [builtin-scalar-realm-id])

(defn builtin-scalar?
  [thing]
  (satisfies? builtin-scalar-realm thing))

; FIXME: natural positive-int
(def int (struct-map builtin-scalar-realm description "int" builtin-scalar-realm-id :int metadata {}))
; FIXME? long?
(def bigdec (struct-map builtin-scalar-realm description "big decimal" builtin-scalar-realm-id :bigdec metadata {}))
(def float (struct-map builtin-scalar-realm description  "float" builtin-scalar-realm-id :float metadata {}))
(def double (struct-map builtin-scalar-realm description "double" builtin-scalar-realm-id :double metadata {}))
(def keyword (struct-map builtin-scalar-realm description "keyword" builtin-scalar-realm-id :keyword metadata {}))
(def symbol (struct-map builtin-scalar-realm description "symbol" builtin-scalar-realm-id :symbol metadata {}))
(def string (struct-map builtin-scalar-realm description "string" builtin-scalar-realm-id :string metadata {}))
(def any (struct-map builtin-scalar-realm description "any" builtin-scalar-realm-id :any metadata {}))

(def scalar-realms
  (into {}
        (map (fn [scalar-realm]
               [(builtin-scalar-realm-id scalar-realm) scalar-realm])
             [int bigdec float double
              keyword symbol string
              any])))

(def-struct ^{:doc "Realm only defined through a predicate."}
  predicate-realm
  :extends Realm
  [predicate-realm-predicate])

(defn predicate?
  [thing]
  (satisfies? predicate-realm thing))

(defn predicate
  [desc predicate]
  (struct-map predicate-realm
              description desc
              predicate-realm-predicate predicate
              metadata {}))

(def-struct ^{:doc "Realm of optional values."}
  optional-realm
  :extends Realm
  [optional-realm-realm])

(defn optional?
  [thing]
  (satisfies? optional-realm thing))

(declare compile)

(defn optional
  [realm]
  (struct-map optional-realm
              description (str "optional " (description realm))
              optional-realm-realm (compile realm)
              metadata {}))

(def-struct ^{:doc "Realm for integer ranges."}
  integer-from-to-realm
  :extends Realm
  [integer-from-to-realm-from integer-from-to-realm-to])

(defn integer-from-to?
  [thing]
  (satisfies? integer-from-to-realm thing))

(defn integer-from-to
  [from to]
  (struct-map integer-from-to-realm
              description (str "integer from " from " to " to)
              integer-from-to-realm-from from
              integer-from-to-realm-to to
              metadata {}))

(defn realm-seq-description
  [realms]
  (str "["
       (string/join ", " (map description realms))
       "]"))

(def-struct ^{:doc "Realm for mixed data."}
  mixed-realm
  :extends Realm
  [mixed-realm-realms])

(defn mixed?
  [thing]
  (satisfies? mixed-realm thing))

(defn mixed
  [& realms]
  (struct-map mixed-realm
              description (str "mixed of " (realm-seq-description realms))
              mixed-realm-realms (map compile realms)
              metadata {}))

;; TODO: intersection-realm

(def-struct^{:doc "Realm for enumerations."}
  enum-realm
  :extends Realm
  [enum-realm-values])

(defn enum?
  [thing]
  (satisfies? enum-realm thing))

(defn enum
  [& values]
  (struct-map enum-realm
              description (str "enumeration of [" (string/join ", " (map str values)) "]")
              enum-realm-values values
              metadata {}))

(def-struct ^{:doc "Realm for sequences."}
  seq-of-realm
  :extends Realm
  [seq-of-realm-realm])

(defn seq-of?
  [thing]
  (satisfies? seq-of-realm thing))

(defn seq-of
  [realm]
  (struct-map seq-of-realm
              description (str "sequence of " (description realm))
              seq-of-realm-realm (compile realm)
              metadata {}))

(def-struct ^{:doc "Realm for sets."}
  set-of-realm
  :extends Realm
  [set-of-realm-realm])

(defn set-of?
  [thing]
  (satisfies? set-of-realm thing))

(defn set-of
  [realm]
  (struct-map set-of-realm
              description (str "set of " (description realm))
              set-of-realm-realm (compile realm)
              metadata{}))

(def-struct ^{:doc "Realm for maps with certain constant keys."}
  map-with-keys-realm
  :extends Realm
  [map-with-keys-realm-map])

(defn map-with-keys?
  [thing]
  (satisfies? map-with-keys-realm thing))

(defn map-with-keys
  [keys-realm-map]
  (struct-map map-with-keys-realm
              description (str "map with keys {"
                               (string/join ", "
                                            (core/map (fn [[key realm]]
                                                        (str key " -> " (description realm)))
                                                      keys-realm-map))
                               "}")
              map-with-keys-realm-map (into {}
                                            (map (fn [[key realm]]
                                                   [key (compile realm)])
                                                 keys-realm-map))
              metadata {}))


(def-struct ^{:doc "Realm for maps with realms for keys and values respectively."}
  map-of-realm
  :extends Realm
  [map-of-realm-key-realm map-of-realm-value-realm])

(defn map-of?
  [thing]
  (satisfies? map-of-realm thing))

(defn map-of
  [key-realm value-realm]
  (struct-map map-of-realm
              description (str "map of {"
                               (description key-realm)
                               " -> "
                               (description value-realm)
                               "}")
              map-of-realm-key-realm (compile key-realm)
              map-of-realm-value-realm (compile value-realm)
              metadata {}))

(def-struct ^{:doc "Realm for tuples, i.e. sequences with a fixed number of elements, each of which has a realm."}
  tuple-realm
  :extends Realm
  [tuple-realm-realms])

(defn tuple?
  [thing]
  (satisfies? tuple-realm thing))

(defn tuple
  [& realms]
  (struct-map tuple-realm
              description (str "tuple of ("
                               (string/join ", " (map description realms))
                               ")")
              tuple-realm-realms (map compile realms)
              metadata {}))

(def-struct ^{:doc "Description of the field of a record."}
  record-realm-field
  [record-realm-field-name record-realm-field-realm record-realm-field-lens])

(defn field
  [name realm lens]
  (struct-map record-realm-field
              record-realm-field-name name
              record-realm-field-realm (compile realm)
              record-realm-field-lens lens))

(def-struct ^{:doc "Realm for records."}
  record-realm
  :extends Realm
  [record-realm-name
   record-realm-constructor
   record-realm-predicate
   record-realm-fields])

(defn record?
  [thing]
  (satisfies? record-realm thing))

(defn record
  [name constructor predicate fields]
  (struct-map record-realm
              description (str "record " name
                               " with fields "
                               (string/join ", "
                                            (map (fn [field]
                                                   (str (record-realm-field-name field) " from realm "
                                                        (description (record-realm-field-realm field))))
                                                 fields)))
              record-realm-name name
              record-realm-constructor constructor
              record-realm-predicate predicate
              record-realm-fields fields
              metadata {}))

(defn compile
  [shorthand]
  (if (realm? shorthand)
    shorthand
    (condp identical? shorthand
      core/int int
      core/bigdec bigdec
      core/float float
      core/double double
      core/keyword keyword
      core/symbol symbol
      core/str string

      (cond
        (fn? shorthand)
        (predicate "unknown predicate" shorthand)

        (vector? shorthand)
        (if (= 1 (count shorthand))
          (seq-of (first shorthand))
          (apply tuple shorthand))

        (and (set? shorthand)
             (= 1 (count shorthand)))
        (set-of (first shorthand))

        (map? shorthand)
        (if (= (count shorthand) 1)
          (let [[key value] (first shorthand)]
            (map-of key value))
          (map-with-keys shorthand))
        
        :else (throw (Exception. (str "unknown realm shorthand: " shorthand)))))))
