(ns active.data.realm
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

(defn contains?
  [realm x]
  ((predicate realm) x))

(defn realm?
  [thing]
  (is-a? Realm thing))

(defn with-metadata
  [realm key data]
  (assoc realm metadata
         (assoc (metadata realm) key data)))

(def-record ^{:doc "Builtin scalar realm."}
  builtin-scalar-realm
  :extends Realm
  [builtin-scalar-realm-id])

(defn builtin-scalar?
  [thing]
  (is-a? builtin-scalar-realm thing))

(defn- natural?
  "Returns true if n is a natural number.
  I.e. a n integer >= 0."
  [n]
  (and (integer? n)
       (>= n 0)))

(defn- real?
  "Returns true if n is a real number."
  [n]
  (number? n))

(def natural (builtin-scalar-realm builtin-scalar-realm-id :natural
                                   predicate natural?
                                   description "natural" metadata {}))
(def integer (builtin-scalar-realm builtin-scalar-realm-id :integer
                                   predicate integer?
                                   description "integer" metadata {}))
#?(:clj (def rational (builtin-scalar-realm builtin-scalar-realm-id :rational
                                            predicate rational?
                                            description "rational" metadata {})))
(def number (builtin-scalar-realm builtin-scalar-realm-id :number
                                  predicate number?
                                  description "number" metadata {}))
(def char (builtin-scalar-realm builtin-scalar-realm-id :char
                                predicate char?
                                description "char" metadata {}))
(def keyword (builtin-scalar-realm builtin-scalar-realm-id :keyword
                                   predicate keyword?
                                   description "keyword" metadata {}))
(def symbol (builtin-scalar-realm builtin-scalar-realm-id :symbol
                                  predicate symbol?
                                  description "symbol" metadata {}))
(def string (builtin-scalar-realm builtin-scalar-realm-id :string
                                  predicate string?
                                  description "string"  metadata {}))
(def boolean (builtin-scalar-realm builtin-scalar-realm-id :boolean
                                   predicate boolean?
                                   description "boolean" metadata {}))
(def uuid (builtin-scalar-realm builtin-scalar-realm-id :uuid
                                predicate uuid?
                                description "uuid" metadata {}))
(def any (builtin-scalar-realm builtin-scalar-realm-id :any
                               predicate any?
                               description "any" metadata {}))

#_(def scalar-realms
  (into {}
        (map (fn [scalar-realm]
               [(builtin-scalar-realm-id scalar-realm) scalar-realm])
             [natural integer #?(:clj rational) real number
              keyword symbol string uuid
              any])))

(def-record ^{:doc "Realm only defined through a predicate."}
  from-predicate-realm
  :extends Realm
  [])

(defn from-predicate?
  [thing]
  (is-a? from-predicate-realm thing))

(defn from-predicate
  [desc pred]
  (from-predicate-realm description desc
                        predicate pred
                        metadata {}))

(def-record ^{:doc "Realm of optional values."}
  optional-realm
  :extends Realm
  [optional-realm-realm])

(defn optional?
  [thing]
  (is-a? optional-realm thing))

(declare compile)

(defn optional
  [realm]
  (let [realm (compile realm)]
    (optional-realm description (str "optional " (description realm))
                    predicate (let [inner-predicate (predicate realm)]
                                (fn [x]
                                  (or (nil? x)
                                      (inner-predicate x))))
                    optional-realm-realm realm
                    metadata {})))

(def-record ^{:doc "Realm for integer ranges."}
  integer-from-to-realm
  :extends Realm
  ; FIXME: bounds optional, have this cover natural and integer
  [integer-from-to-realm-from integer-from-to-realm-to])

(defn integer-from-to?
  [thing]
  (is-a? integer-from-to-realm thing))

(defn integer-from-to
  [from to]
  (integer-from-to-realm description (str "integer from " from " to " to)
                         integer-from-to-realm-from from
                         integer-from-to-realm-to to
                         predicate (fn [x]
                                     (and (integer? x)
                                          (>= x from)
                                          (<= x to)))
                         metadata {}))

(def-record  ^{:doc "Realm for integer ranges."}
  real-range-realm
  :extends Realm
  ; clusive is either :in or :ex
  [real-range-realm-clusive-left
   real-range-realm-left ; may be nil
   real-range-realm-right
   real-range-realm-clusive-right])

(defn- make-real-range
  [clusive-left left right clusive-right]
  (real-range-realm description (if (and (nil? left) (nil? right))
                                   "real"
                                   (str "real range "
                                        (case clusive-left
                                          (:in) "["
                                          (:ex) "(")
                                        (or left "")
                                        ", "
                                        (or right "")
                                        (case clusive-right
                                          (:in) "]"
                                          (:ex) ")")))
                     real-range-realm-clusive-left clusive-left
                     real-range-realm-left left
                     real-range-realm-right right
                     real-range-realm-clusive-right clusive-right
                     predicate (cond
                                 (and left right)
                                 (case clusive-left
                                   (:in) (case clusive-right
                                           (:in)
                                           (fn [n]
                                             (and (real? n)
                                                  (<= left n right)))
                                           (:ex)
                                           (fn [n]
                                             (and (real? n)
                                                  (<= left n)
                                                  (< n right))))
                                   (:ex) (case clusive-right
                                           (:in)
                                           (fn [n]
                                             (and (real? n)
                                                  (< left n)
                                                  (<= n right)))
                                           (:ex)
                                           (fn [n]
                                             (and (real? n)
                                                  (< left n right)))))

                                 left
                                 (case clusive-left
                                   (:in)
                                   (fn [n]
                                     (and (real? n)
                                          (<= left n)))
                                   (:ex)
                                   (fn [n]
                                     (and (real? n)
                                          (< left n))))

                                 right
                                 (case clusive-right
                                   (:in)
                                   (fn [n]
                                     (and (real? n)
                                          (<= n right)))
                                   (:ex)
                                   (fn [n]
                                     (and (real? n)
                                          (< n right))))

                                 :else
                                 real?)
                     metadata {}))

(defn real-range
  ([clusive-left left right clusive-right]
   (make-real-range clusive-left left right clusive-right))
  ([lr1 lr2]
   (case lr1
     (:in :ex) (make-real-range lr1 lr2 nil :ex)
     (case lr2
       (:in :ex) (make-real-range :ex nil lr1 lr2)))))

(defn real-range?
  [thing]
  (is-a? real-range? thing))

(def real (real-range :in nil nil :in))

(defn- realm-seq-description
  [realms]
  (str "["
       (string/join ", " (map description realms))
       "]"))

(def-record ^{:doc "Realm for unions."}
  union-realm
  :extends Realm
  ;; the shallow predicates of these should identify pairwise disjoint sets
  [union-realm-realms])

(defn union?
  [thing]
  (is-a? union-realm thing))

(defn union
  [& realms]
  (union-realm description (str "union of " (realm-seq-description realms))
               union-realm-realms (map compile realms)
               predicate (let [predicates (map predicate realms)]
                           (fn [x]
                             (core/boolean (some #(% x) predicates))))
               metadata {}))

(def-record^{:doc "Realm for enumerations."}
  enum-realm
  :extends Realm
  [enum-realm-values]) ; set

(defn enum?
  [thing]
  (is-a? enum-realm thing))

(defn enum
  [& values]
  (let [values-set (set values)]
    (enum-realm description (str "enumeration of [" (string/join ", " (map str values)) "]")
                enum-realm-values values-set
                predicate (fn [x]
                            (core/contains? values-set x))
                metadata {})))

(def-record ^{:doc "Realm for intersections."}
  intersection-realm
  :extends Realm
  ;; the shallow predicates of these should identify pairwise disjoint sets
  [intersection-realm-realms])

(defn intersection?
  [thing]
  (is-a? intersection-realm thing))

(defn intersection
  [& realms]
  (intersection-realm description (str "intersection of " (realm-seq-description realms))
                      intersection-realm-realms (map compile realms)
                      predicate (let [predicates (map predicate realms)]
                                  (fn [x]
                                    (every? #(% x) predicates)))
                      metadata {}))

(def-record ^{:doc "Realm for sequences."}
  sequence-of-realm
  :extends Realm
  [sequence-of-realm-realm])

(defn sequence-of?
  [thing]
  (is-a? sequence-of-realm thing))

(defn sequence-of
  [realm]
  (let [realm (compile realm)]
    (sequence-of-realm description (str "sequence of " (description realm))
                       sequence-of-realm-realm realm
                       predicate sequential?
                       metadata {})))

(def-record ^{:doc "Realm for sets."}
  set-of-realm
  :extends Realm
  [set-of-realm-realm])

(defn set-of?
  [thing]
  (is-a? set-of-realm thing))

(defn set-of
  [realm]
  (let [realm (compile realm)]
    (set-of-realm description (str "set of " (description realm))
                  set-of-realm-realm realm
                  predicate set?
                  metadata {})))

;; keys you can live out just have optional realms

(def-record ^{:doc "Realm for maps with certain constant keys."}
  map-with-keys-realm
  :extends Realm
  [map-with-keys-realm-map])

(defn map-with-keys?
  [thing]
  (is-a? map-with-keys-realm thing))

(defn map-with-keys
  [keys-realm-map]
  (let [keys-realm-map (into {}
                             (map (fn [[key realm]]
                                    [key (compile realm)])
                                  keys-realm-map))]
    (map-with-keys-realm description (str "map with keys {"
                                          (string/join ", "
                                                       (core/map (fn [[key realm]]
                                                                   (str key " -> " (description realm)))
                                                                 keys-realm-map))
                                          "}")
                         map-with-keys-realm-map keys-realm-map
                         predicate map?
                         metadata {})))

(def-record ^{:doc "Realm for maps with realms for keys and values respectively."}
  map-of-realm
  :extends Realm
  [map-of-realm-key-realm map-of-realm-value-realm])

(defn map-of?
  [thing]
  (is-a? map-of-realm thing))

(defn map-of
  [key-realm value-realm]
  (let [key-realm (compile key-realm)
        value-realm (compile value-realm)]
    (map-of-realm description (str "map of {"
                                   (description key-realm)
                                   " -> "
                                   (description value-realm)
                                   "}")
                  map-of-realm-key-realm key-realm
                  map-of-realm-value-realm value-realm
                  predicate map?
                  metadata {})))

(def-record ^{:doc "Realm for tuples, i.e. sequences with a fixed number of elements, each of which has a realm."}
  tuple-realm
  :extends Realm
  [tuple-realm-realms])

(defn tuple?
  [thing]
  (is-a? tuple-realm thing))

(defn tuple
  [& realms]
  (let [realms (mapv compile realms)]
    (tuple-realm description (str "tuple of ("
                                  (string/join ", " (map description realms))
                                  ")")
                 tuple-realm-realms realms
                 predicate (let [size (count realms)]
                             (fn [x]
                               (and (sequential? x)
                                    (= (count x) size))))
                 metadata {})))

(def-record ^{:doc "Description of the field of a record."}
  record-realm-field
  [record-realm-field-name record-realm-field-realm record-realm-field-getter])

(defn field
  [name realm getter]
  (record-realm-field record-realm-field-name name
                      record-realm-field-realm (compile realm)
                      record-realm-field-getter getter))

(def-record ^{:doc "Realm for records."}
  record-realm
  :extends Realm
  [record-realm-name
   record-realm-constructor
   record-realm-fields])

(defn record?
  [thing]
  (is-a? record-realm thing))

(defn record
  [name constructor pred fields]
  (record-realm description (str "record " name
                                 " with fields "
                                 (string/join ", "
                                              (map (fn [field]
                                                     (str (record-realm-field-name field) " from realm "
                                                          (description (record-realm-field-realm field))))
                                                   fields)))
                record-realm-name name
                record-realm-constructor constructor
                predicate pred
                record-realm-fields fields
                metadata {}))

(defn ^:no-doc create-realm-struct-realm
  "Creates and returns a record-realm for the given struct.
  If field-realm-map does not specify a realm for a field, [[any]] is used."
  [struct field-realm-map]
  (record 'unnamed-struct
          (struct/constructor struct)
          (fn [v] (and (map? v) (struct/has-keys? struct v)))
          (map (fn [key]
                 (field (core/symbol (str key))
                        (get field-realm-map key any)
                        (struct/accessor struct key)))
               (struct/struct-keys struct))))

(defn ^:no-doc create-realm-record-realm
  "Creates and returns a record-realm for the given struct.
  If field-realm-map does not specify a realm for a field, [[any]] is used."
  [rec field-realm-map]
  (record (record/record-name rec)
          (record/constructor rec)
          (partial is-a? rec)
          (map (fn [key]
                 (field (core/symbol (str key))
                        (get field-realm-map key any)
                        key))
               (record/record-keys rec))))

(defn ^:no-doc struct->record-realm
  "Returns a realm for a struct."
  [struct]
  (create-realm-struct-realm struct {}))

(defn ^:no-doc record->record-realm
  "Returns a realm for a record."
  [record]
  (or (get (meta record) realm-record-meta/record-realm-meta-key)
      (create-realm-record-realm record {})))

(def-record ^{:doc "Function case."}
  function-case
  [function-case-positional-argument-realms ; seq of realms
   function-case-optional-arguments-realm ; nil or sequence-of realm or map-with-keys realm or tuple realm
   function-case-return-realm])

(defn- function-case-description
  [function-case]
  (str "function ("
       (string/join ", "
                    (map description (function-case-positional-argument-realms function-case)))
       (if-let [optional (function-case-optional-arguments-realm function-case)]
         (str " & "
              (description optional))
         "")

       ") -> "
       (description (function-case-return-realm function-case))))
   
; list is expected to start with ->
(defn- parse-function-return
  [shorthand list]
  (when (not (= (first list) '->))
    (throw (ex-info (str "function realm has no arrow: " shorthand) {:shorthand shorthand})))
  (when (empty? (rest list))
    (throw (ex-info (str "function realm has nothing after ->: " shorthand) {:shorthand shorthand})))
  (when (not (empty? (rest (rest list))))
    (throw (ex-info (str "function realm has more then one thing after ->: " shorthand) {:shorthand shorthand})))
  (first (rest list)))

;; (r1 r2 r3 -> r)
;; (r1 r2 r3 & (rs) -> r)
;; (r1 r2 r3 & [rr1 rr2]) -> r)
;; (r1 r2 r3 & {:a ra :b rb :c rc} -> r)
(defn- compile-function-case-shorthand
  [shorthand]
  (loop [list shorthand
         positional (transient [])]
    (if (empty? list)
      (throw (ex-info (str "function realm does not have arrow: " shorthand) {:shorthand shorthand}))

      (let [f (first list)]
        (case f
          (->)
          (let [positional (persistent! positional)
                return `(compile ~(parse-function-return shorthand list))]
            `(let [positional# ~positional
                   return# ~return]
               (function-cases ; hack to get description right
                (function-realm function-realm-cases
                                [(function-case function-case-positional-argument-realms positional#
                                                function-case-optional-arguments-realm nil
                                                function-case-return-realm return#)]))))
          (&)
          (if (empty? (rest list))
            (throw (ex-info (str "function realm does not have an arrow: " shorthand) {:shorthand shorthand}))

            (let [list (rest list)
                  f (first list)
                  return `(compile ~(parse-function-return shorthand (rest list)))
                  positional (persistent! positional)
                  optional (cond
                             (list? f)
                             (do
                               (when (empty? (rest list))
                                 (throw (ex-info (str "in function realm, empty optional list realm: " shorthand) {:shorthand shorthand})))
                               (when (not (empty (rest (rest list))))
                                 (throw (ex-info (str "in function realm, more than one optional list realm: " shorthand) {:shorthand shorthand})))
                               `(sequence-of (compile ~(first f))))

                             (vector? f)
                             `(tuple ~@(mapv (fn [o]
                                               `(compile ~o))
                                             f))

                             (map? f)
                             `(map-with-keys ~(into {}
                                                    (map (fn [[key realm]]
                                                           [key `(compile ~realm)])
                                                         f)))

                             :else
                             (throw (ex-info (str "function realm has garbage after &: " shorthand) {:shorthand shorthand})))]
              `(let [positional# ~positional
                     optional# ~optional
                     return# ~return]
                 (function-cases ; hack to get description right
                  (function-realm function-realm-cases
                                  [(function-case function-case-positional-argument-realms positional#
                                                  function-case-optional-arguments-realm optional#
                                                  function-case-return-realm return#)])))))

          (recur (rest list)
                 (conj! positional `(compile ~f))))))))

(defmacro function
  "Shorthand for function realms.

Here are the different forms:

(function r1 r2 r3 -> r)                       - fixed arity
(function r1 r2 r3 & (rs) -> r)                - rest args
(function r1 r2 r3 & [rr1 rr2]) -> r)          - 2 optional args
(function r1 r2 r3 & {:a ra :b rb :c rc} -> r) - optional keyword args"
  [& shorthand]
  (compile-function-case-shorthand shorthand))

(def-record ^{:doc "Realm for function with multiple cases."}
  function-realm
  :extends Realm
  [function-realm-cases])

(defn ^:no-doc function-cases
  "Just call this on a buch of function realms."
  [& cases]
  (let [cases (mapcat function-realm-cases cases)]
    (function-realm function-realm-cases cases
                    metadata {}
                    predicate fn?
                    description
                    (if (= (count cases) 1)
                      (function-case-description (first cases))
                      (str "function with cases " (string/join ", " (map function-case-description cases)))))))

(defn function?
  [thing]
  (is-a? function-realm thing))

(def-record ^{:doc "Lazy realm."}
  delayed-realm
  :extends Realm
  [delayed-realm-delay])

(defmacro delay
  [realm-expression]
  `(let [delay-object# (core/delay (compile ~realm-expression))]
     (delayed-realm delayed-realm-delay delay-object#
                    metadata {}
                    predicate (fn [x#]
                                ((predicate (force delay-object#)) x#))
                    description "delayed realm")))

(defn delayed?
  [thing]
  (is-a? delayed-realm thing))

; for documenting polymorphism
(def-record ^{:doc "Named realm."}
  named-realm
  :extends Realm
  [named-realm-name ; keyword
   named-realm-realm])

(defn named
  [name realm]
  (let [realm (compile realm)]
    (named-realm named-realm-name name
                 named-realm-realm realm
                 metadata {}
                 predicate (predicate realm)
                 description (str "realm named " name ": " (description realm)))))

(defn named?
  [thing]
  (is-a? named-realm thing))

; FIXME: why not intersection + predicate?
(def-record ^{:doc "Realm restricted by a predicate"}
  restricted-realm
  :extends Realm
  [restricted-realm-realm
   restricted-realm-predicate])

(defn restricted
  [realm pred predicate-description]
  (let [realm (compile realm)]
    (restricted-realm restricted-realm-realm realm
                      restricted-realm-predicate pred
                      metadata {}
                      predicate (let [realm-predicate (predicate realm)]
                                  (fn [x]
                                    (and (realm-predicate x)
                                         (pred x))))
                      description (str (description realm) " restricted to " predicate-description))))

(defn restricted?
  [thing]
  (is-a? restricted-realm thing))

(defn compile
  [shorthand]
  (if (realm? shorthand)
    shorthand
    (cond
      (fn? shorthand)
      (from-predicate "unknown predicate" shorthand) ; FIXME: questionable

      (vector? shorthand)
      (if (= 1 (count shorthand))
        (sequence-of (first shorthand))
        (apply tuple shorthand))

      (and (set? shorthand)
           (= 1 (count shorthand)))
      (set-of (first shorthand))

      (map? shorthand)
      (if (= (count shorthand) 1)
        (let [[key value] (first shorthand)]
          (map-of key value))
        (map-with-keys shorthand))

      (record/record? shorthand)
      (record->record-realm shorthand)
        
      (struct/struct? shorthand)
      (struct->record-realm shorthand)

      (keyword? shorthand)
      (named shorthand any)

      :else
      (throw (ex-info (str "unknown realm shorthand: " (pr-str shorthand))
                      {::unknown-realm-shorthand shorthand})))))



