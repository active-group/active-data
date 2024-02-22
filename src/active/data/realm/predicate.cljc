(ns active.data.realm.predicate
  (:refer-clojure :exclude [contains?])
  (:require
   #?(:clj [clojure.core :as core]
      :cljs [cljs.core :as core])
   [active.data.realm :as realm]))

(defn shallow-predicate
  "Return a shallow predicate for the realm.

  This does not perform a full check whether a value conforms to a realm,
  but is rather intended only to figure out which one of a set of realms
  a value belongs to, and quickly.

  In particular, the predicate does not consider the realms of the
  contents of collections and records.

  It does, however, distinguish tuples of different sizes."
  [realm]
  (cond
    (realm/builtin-scalar? realm)
    (realm/builtin-scalar-realm-predicate realm)
    
    (realm/predicate? realm)
    (realm/predicate-realm-predicate realm)
    
    (realm/optional? realm)
    (let [inner-predicate (shallow-predicate (realm/optional-realm-realm realm))]
      (fn [x]
        (or (nil? x)
            (inner-predicate x))))
    
    (realm/integer-from-to? realm)
    (let [from (realm/integer-from-to-realm-from realm)
          to (realm/integer-from-to-realm-to realm)]
      (fn [x]
        (and (integer? x)
             (>= x from)
             (<= x to))))
    
    (realm/union? realm)
    (let [predicates (map shallow-predicate (realm/union-realm-realms realm))]
      (fn [x]
        (boolean (some #(% x) predicates))))
    
    (realm/intersection? realm)
    (let [predicates (map shallow-predicate (realm/intersection-realm-realms realm))]
      (fn [x]
        (every? #(% x) predicates)))
    
    (realm/enum? realm) (let [set (realm/enum-realm-values realm)]
                          (fn [x]
                            (core/contains? set x)))
    (realm/sequence-of? realm) sequential?
    (realm/set-of? realm) set?
    (realm/map-with-keys? realm) map?
    (realm/map-of? realm) map?

    (realm/tuple? realm)
    (let [size (count (realm/tuple-realm-realms realm))]
      (fn [x]
        (and (sequential? x)
             (= (count x) size))))

    (realm/record? realm) (realm/record-realm-predicate realm)

    (realm/function? realm) fn?

    (realm/delayed? realm) (shallow-predicate (force (realm/delayed-realm-delay realm)))

    (realm/named? realm)
    (shallow-predicate (realm/named-realm-realm realm))

    (realm/restricted? realm) ; debatable
    (let [predicate (realm/restricted-realm-predicate realm)
          realm-predicate (shallow-predicate (realm/restricted-realm-realm realm))]
      (fn [thing]
        (and (realm-predicate thing)
             (predicate thing))))))

(defn contains?
  [realm x]
  ((shallow-predicate realm) x)) ; FIXME: could this be more efficient?




