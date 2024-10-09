(ns ^{:doc "Predicates, selectors, and realms for processing realm values as input."}
  active.data.realm.inspection
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

(defn integer-from-to?
  [thing]
  (is-a? integer-from-to-realm thing))

(re-export
 realm-records/real-range-realm
 realm-records/real-range-realm-clusive-left
 realm-records/real-range-realm-left
 realm-records/real-range-realm-right
 realm-records/real-range-realm-clusive-right)

(defn real-range?
  [thing]
  (is-a? real-range-realm thing))

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

(defn- the-builtin-scalar [realm]
  ;; Note: using (enum realm) would work usually, but not when metadata are added to the base value.
  ;; That's why we need a predicate that checks if it's also a builting-scalar-realm and with the same id.
  (assert (is-a? realm-records/builtin-scalar-realm realm))
  (let [this-id (realm-records/builtin-scalar-realm-id realm)]
    (realm/from-predicate (realm-records/description realm)
                          (fn [r]
                            (and (is-a? realm-records/builtin-scalar-realm r)
                                 (= (realm-records/builtin-scalar-realm-id r)
                                    this-id))))))

#?(:clj (def ^{:doc "Realm containg the rational realm."} rational (the-builtin-scalar realm/rational)))
(def ^{:doc "Realm containg the number realm."} number (the-builtin-scalar realm/number))
(def ^{:doc "Realm containg the char realm."} char (the-builtin-scalar realm/char))
(def ^{:doc "Realm containg the keyword realm."} keyword (the-builtin-scalar realm/keyword))
(def ^{:doc "Realm containg the symbol realm."} symbol (the-builtin-scalar realm/symbol))
(def ^{:doc "Realm containg the string realm."} string (the-builtin-scalar realm/string))
(def ^{:doc "Realm containg the boolean realm."} boolean (the-builtin-scalar realm/boolean))
(def ^{:doc "Realm containg the uuid realm."} uuid (the-builtin-scalar realm/uuid))
(def ^{:doc "Realm containing the any realm."} any (the-builtin-scalar realm/any))

(def ^{:doc "Realm containing predicate realms."}
  from-predicate
  (realm/record->record-realm realm-records/from-predicate-realm))
(def ^{:doc "Realm containing optional realms."}
  optional
  (realm/record->record-realm realm-records/optional-realm))
(def ^{:doc "Realm containing integer-from-to realms."}
  integer-from-to
  (realm/record->record-realm realm-records/integer-from-to-realm))
(def ^{:doc "Realm containing real-range realms."}
  real-range
  (realm/record->record-realm realm-records/real-range-realm))
(def ^{:doc "Realm containing union realms."}
  union
  (realm/record->record-realm realm-records/union-realm))
(def ^{:doc "Realm containing intersection realms."}
  intersection
  (realm/record->record-realm realm-records/intersection-realm))
(def ^{:doc "Realm containing enum realms."}
  enum
  (realm/record->record-realm realm-records/enum-realm))
(def ^{:doc "Realm containing sequence-of realms."}
  sequence-of
  (realm/record->record-realm realm-records/sequence-of-realm))
(def ^{:doc "Realm containing set-of realms."}
  set-of
  (realm/record->record-realm realm-records/set-of-realm))
(def ^{:doc "Realm containing map-with-keys realms."}
  map-with-keys
  (realm/record->record-realm realm-records/map-with-keys-realm))
(def ^{:doc "Realm containing map-with-tag realms."}
  map-with-tag
  (realm/record->record-realm realm-records/map-with-tag-realm))
(def ^{:doc "Realm containing map-of realms."}
  map-of
  (realm/record->record-realm realm-records/map-of-realm))
(def ^{:doc "Realm containing tuple realms."}
  tuple
  (realm/record->record-realm realm-records/tuple-realm))
(def ^{:doc "Realm containing record realms."}
  record
  (realm/record->record-realm realm-records/record-realm))
(def ^{:doc "Realm containing function realms."}
  function
  (realm/record->record-realm realm-records/function-realm))
(def ^{:doc "Realm containing delayed realms."}
  delayed
  (realm/record->record-realm realm-records/delayed-realm))
(def ^{:doc "Realm containing named realms."}
  named
  (realm/record->record-realm realm-records/named-realm))

(def ^{:doc "Realm containing all realms"} realm
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
   
