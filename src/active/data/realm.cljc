(ns active.data.realm
  (:refer-clojure :exclude [int bigdec float double keyword symbol boolean seq compile record? delay delayed?
                            struct-map instance? satisfies? set-validator!])
  (:require
   [clojure.core :as core]
   [active.data.struct :refer [def-struct struct-map satisfies? instance?]]
   [active.data.struct :as struct]
   [active.data.struct.closed-struct :as closed-struct]
   [active.data.struct.closed-struct-meta :as closed-struct-meta]
   [clojure.set :as set]
   [clojure.string :as string]))

(def-struct Realm [description metadata])

(defn realm?
  [thing]
  (satisfies? Realm thing))

;; FIXME: should there be a fold/generic dispatch?
;; FIXME: realm realm ...
;; FIXME: put predicate into builtin

(def-struct ^{:doc "Builtin scalar realm."}
  builtin-scalar-realm
  :extends Realm
  [builtin-scalar-realm-id])

(defn builtin-scalar?
  [thing]
  (satisfies? builtin-scalar-realm thing))

; FIXME: natural positive-int
(def int (struct-map builtin-scalar-realm description "int" builtin-scalar-realm-id :int metadata {}))
; FIXME? long? number?
(def bigdec (struct-map builtin-scalar-realm description "big decimal" builtin-scalar-realm-id :bigdec metadata {}))
(def float (struct-map builtin-scalar-realm description  "float" builtin-scalar-realm-id :float metadata {}))
(def double (struct-map builtin-scalar-realm description "double" builtin-scalar-realm-id :double metadata {}))
(def keyword (struct-map builtin-scalar-realm description "keyword" builtin-scalar-realm-id :keyword metadata {}))
(def symbol (struct-map builtin-scalar-realm description "symbol" builtin-scalar-realm-id :symbol metadata {}))
(def string (struct-map builtin-scalar-realm description "string" builtin-scalar-realm-id :string metadata {}))
(def boolean (struct-map builtin-scalar-realm description "boolean" builtin-scalar-realm-id :boolean metadata {}))
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
  (instance? predicate-realm thing))

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
  (instance? optional-realm thing))

(declare compile)

(defn optional
  [realm]
  (let [realm (compile realm)]
    (struct-map optional-realm
                description (str "optional " (description realm))
                optional-realm-realm realm
                metadata {})))

(def-struct ^{:doc "Realm for integer ranges."}
  integer-from-to-realm
  :extends Realm
  [integer-from-to-realm-from integer-from-to-realm-to])

(defn integer-from-to?
  [thing]
  (instance? integer-from-to-realm thing))

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

(def-struct ^{:doc "Realm for unions."}
  union-realm
  :extends Realm
  ;; the shallow predicates of these should identify pairwise disjoint sets
  [union-realm-realms])

(defn union?
  [thing]
  (instance? union-realm thing))

(defn union
  [& realms]
  (struct-map union-realm
              description (str "union of " (realm-seq-description realms))
              union-realm-realms (map compile realms)
              metadata {}))

(def-struct^{:doc "Realm for enumerations."}
  enum-realm
  :extends Realm
  [enum-realm-values]) ; set

(defn enum?
  [thing]
  (instance? enum-realm thing))

(defn enum
  [& values]
  (struct-map enum-realm
              description (str "enumeration of [" (string/join ", " (map str values)) "]")
              enum-realm-values (set values)
              metadata {}))

(def-struct ^{:doc "Realm for intersections."}
  intersection-realm
  :extends Realm
  ;; the shallow predicates of these should identify pairwise disjoint sets
  [intersection-realm-realms])

(defn intersection?
  [thing]
  (instance? intersection-realm thing))

(defn intersection
  [& realms]
  (struct-map intersection-realm
              description (str "intersection of " (realm-seq-description realms))
              intersection-realm-realms (map compile realms)
              metadata {}))

(def-struct ^{:doc "Realm for sequences."}
  sequence-of-realm
  :extends Realm
  [sequence-of-realm-realm])

(defn sequence-of?
  [thing]
  (instance? sequence-of-realm thing))

(defn sequence-of
  [realm]
  (let [realm (compile realm)]
    (struct-map sequence-of-realm
                description (str "sequence of " (description realm))
                sequence-of-realm-realm realm
                metadata {})))

(def-struct ^{:doc "Realm for sets."}
  set-of-realm
  :extends Realm
  [set-of-realm-realm])

(defn set-of?
  [thing]
  (instance? set-of-realm thing))

(defn set-of
  [realm]
  (let [realm (compile realm)]
    (struct-map set-of-realm
                description (str "set of " (description realm))
                set-of-realm-realm realm
                metadata {})))

;; FIXME: always all the keys, or are certain keys optional?
;; maybe do that with optional realms?

(def-struct ^{:doc "Realm for maps with certain constant keys."}
  map-with-keys-realm
  :extends Realm
  [map-with-keys-realm-map])

(defn map-with-keys?
  [thing]
  (instance? map-with-keys-realm thing))

(defn map-with-keys
  [keys-realm-map]
  (let [keys-realm-map (into {}
                             (map (fn [[key realm]]
                                    [key (compile realm)])
                                  keys-realm-map))]
    (struct-map map-with-keys-realm
                description (str "map with keys {"
                                 (string/join ", "
                                              (core/map (fn [[key realm]]
                                                          (str key " -> " (description realm)))
                                                        keys-realm-map))
                                 "}")
                map-with-keys-realm-map keys-realm-map
                metadata {})))

(def-struct ^{:doc "Realm for maps with realms for keys and values respectively."}
  map-of-realm
  :extends Realm
  [map-of-realm-key-realm map-of-realm-value-realm])

(defn map-of?
  [thing]
  (instance? map-of-realm thing))

(defn map-of
  [key-realm value-realm]
  (let [key-realm (compile key-realm)
        value-realm (compile value-realm)]
    (struct-map map-of-realm
                description (str "map of {"
                                 (description key-realm)
                                 " -> "
                                 (description value-realm)
                                 "}")
                map-of-realm-key-realm key-realm
                map-of-realm-value-realm value-realm
                metadata {})))

(def-struct ^{:doc "Realm for tuples, i.e. sequences with a fixed number of elements, each of which has a realm."}
  tuple-realm
  :extends Realm
  [tuple-realm-realms])

(defn tuple?
  [thing]
  (instance? tuple-realm thing))

(defn tuple
  [& realms]
  (let [realms (mapv compile realms)]
    (struct-map tuple-realm
                description (str "tuple of ("
                                 (string/join ", " (map description realms))
                                 ")")
                tuple-realm-realms realms
                metadata {})))

(def-struct ^{:doc "Description of the field of a record."}
  record-realm-field
  [record-realm-field-name record-realm-field-realm record-realm-field-getter])

(defn field
  [name realm getter]
  (struct-map record-realm-field
              record-realm-field-name name
              record-realm-field-realm (compile realm)
              record-realm-field-getter getter))

(def-struct ^{:doc "Realm for records."}
  record-realm
  :extends Realm
  [record-realm-name
   record-realm-constructor
   record-realm-predicate
   record-realm-fields])

(defn record?
  [thing]
  (instance? record-realm thing))

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

(defn struct->record-realm
  "Returns a realm for a struct with the given fields and their reals."
  [struct]
  (record (or (get (meta struct) closed-struct-meta/name-meta-key)
              'unnamed-struct)
          (struct/constructor struct)
          (partial struct/instance? struct)
          (map (fn [[getter realm]]
                 (field (core/symbol (str getter))
                        realm
                        getter))
               (or (get (meta struct) closed-struct-meta/fields-realm-map-meta-key)
                   (into {}
                         (map (fn [key]
                                [key any])
                              (closed-struct/keys struct)))))))

(def-struct ^{:doc "Realm for function."}
  function-realm
  :extends Realm
  [function-realm-positional-argument-realms ; seq of realms
   function-realm-optional-arguments-realm ; sequence-of realm or map-with-keys realm or tuple realm
   function-realm-return-realm])

; list is expected to start with ->
(defn- parse-function-return
  [shorthand list]
  (when (not (= (first list) '->))
    (throw (Exception. (str "function realm has no arrow: " shorthand))))
  (when (empty? (rest list))
    (throw (Exception. (str "function realm has nothing after ->: " shorthand))))
  (when (not (empty? (rest (rest list))))
    (throw (Exception. (str "function realm has more then one thing after ->: " shorthand))))
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
      (throw (Exception. (str "function realm does not have arrow: " shorthand)))

      (let [f (first list)]
        (case f
          (->)
          (let [positional (persistent! positional)
                return `(compile ~(parse-function-return shorthand list))]
            `(let [positional# ~positional
                   return# ~return]
               (struct-map function-realm
                           function-realm-positional-argument-realms positional#
                           function-realm-optional-arguments-realm nil
                           function-realm-return-realm return#
                           metadata {}
                           description (str "function ("
                                            (string/join ", "
                                                         (map description positional#))
                                            ") -> "
                                            (description return#)))))
          (&)
          (if (empty? (rest list))
            (throw (Exception. (str "function realm does not have an arrow: " shorthand)))

            (let [list (rest list)
                  f (first list)
                  return `(compile ~(parse-function-return shorthand (rest list)))
                  positional (persistent! positional)
                  optional (cond
                             (list? f)
                             (do
                               (when (empty? (rest list))
                                 (throw (Exception. (str "in function realm, empty optional list realm: " shorthand))))
                               (when (not (empty (rest (rest list))))
                                 (throw (Exception. (str "in function realm, more than one optional list realm: " shorthand))))
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
                             (throw (Exception. (str "function realm has garbage after &: " shorthand))))]
              `(let [positional# ~positional
                     optional# ~optional
                     return# ~return]
                 (struct-map function-realm
                             function-realm-positional-argument-realms positional#
                             function-realm-optional-arguments-realm optional#
                             function-realm-return-realm return#
                             metadata {}
                             description (str "function ("
                                              (string/join ", "
                                                           (map description positional#))

                                              " & "
                                              (description optional#)
                                              ") -> "
                                              (description return#))))))

          (recur (rest list)
                 (conj! positional `(compile ~f))))))))

(defmacro function
  [& shorthand]
  (compile-function-case-shorthand shorthand))

(def-struct ^{:doc "Realm for function with multiple cases."}
  function-cases-realm
  :extends Realm
  [function-cases-realm-cases])

(defn function-cases
  [& cases]
  (let [cases (mapv compile cases)]
    (struct-map function-cases-realm
                function-cases-realm-cases cases
                metadata {}
                description
                (str "function with cases " (string/join ", " (map description cases))))))

(defn function?
  [thing]
  (or (instance? function-realm thing)
      (instance? function-cases-realm thing)))

(def-struct ^{:doc "Lazy realm."}
  delayed-realm
  :extends Realm
  [delayed-realm-delay])

(defmacro delay
  [realm-expression]
  `(struct-map delayed-realm
               delayed-realm-delay (clojure.core/delay (compile ~realm-expression))
               metadata {}
               description "delayed realm"))

(defn delayed?
  [thing]
  (instance? delayed-realm thing))

(defn core-protocol?
  "Gross hack, as clojure.core/protocol? exists but is private?"
  [thing]
  (try
    (core/satisfies? thing :nope)
    true
    (catch Exception exception false)))

(def-struct ^{:doc "Realm for objects implementing a protocol."}
  protocol-realm
  :extends Realm
  [protocol-realm-protocol])

(defn protocol
  [protocol]
  (struct-map protocol-realm
              protocol-realm-protocol protocol
              metadata {}
              description (str "realm for protocol " (:var protocol))))


(defn protocol?
  [thing]
  (instance? protocol-realm thing))

; for documenting polymorphism
(def-struct ^{:doc "Named realm."}
  named-realm
  :extends Realm
  [named-realm-name ; keyword
   named-realm-realm])

(defn named
  [name realm]
  (let [realm (compile realm)]
    (struct-map named-realm
                named-realm-name name
                named-realm-realm realm
                metadata {}
                description (str "realm named " name ": " (description realm)))))

(defn named?
  [thing]
  (instance? named-realm thing))

(def-struct ^{:doc "Realm restricted by a predicate"}
  restricted-realm
  :extends Realm
  [restricted-realm-realm
   restricted-realm-predicate])

(defn restricted
  [realm predicate predicate-description]
  (let [realm (compile realm)]
    (struct-map restricted-realm
                restricted-realm-realm realm
                restricted-realm-predicate predicate
                metadata {}
                description (str (description realm) " restricted to " predicate-description))))

(defn restricted?
  [thing]
  (instance? restricted-realm thing))

(defn compile
  [shorthand]
  (if (realm? shorthand)
    shorthand
    (condp identical? shorthand
      core/int int
      core/bigdec bigdec
      core/float float
      core/double double
      core/boolean boolean
      core/keyword keyword
      core/symbol symbol
      core/str string

      (cond
        (core-protocol? shorthand)
        (protocol shorthand)
        
        (fn? shorthand)
        (predicate "unknown predicate" shorthand)

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

        (closed-struct/closed-struct? shorthand)
        (or (get (meta shorthand) closed-struct-meta/record-realm-meta-key)
            (struct->record-realm shorthand))

        (keyword? shorthand)
        (named shorthand any)

        :else (throw (ex-info (str "unknown realm shorthand: " shorthand)
                              {::unknown-realm-shorthand shorthand}))))))

(def realm-predicates {'builtin-scalar? `builtin-scalar?
                       'predicate? `predicate?
                       'optional? `optional?
                       'integer-from-to? `integer-from-to?
                       'union? `union?
                       'intersection? `intersection?
                       'enum? `enum?
                       'sequence-of? `sequence-of?
                       'set-of? `set-of?
                       'map-with-keys? `map-with-keys?
                       'map-of? `map-of?
                       'tuple? `tuple?
                       'record? `record?
                       'function? `function?
                       'delayed? `delayed?
                       'protocol? `protocol?
                       'named? `named?
                       'restricted? `restricted?})

(defmacro dispatch
  "(dispatch subject & cases)

where each case has one of the following forms:

predicate expr
:else expr

The predicate must be the unqalified name of a realm predicate, i.e.
builtin-scalar?, optional?, etc.

This will generate a dispatch on subject according to realm type,
where each expr fires when the corresponding predicate returns true.

If there is no explicit :else clause, this will throw an exception on
non-realm values.

This will throw compile-time errors if there are unknown or uncovered
realm cases."
  [?subject & ?clauses]
  (let [pairs (partition 2 ?clauses)
        subject-name `subject#]
    (loop [pairs pairs
           realm-predicates (transient realm-predicates)
           cond-rest (transient [])]
      (if (empty? pairs)
        (let [realm-predicates (persistent! realm-predicates)]
          (if (empty? realm-predicates)
            `(let [~subject-name ~?subject]
               (cond ~@(persistent! cond-rest)
                     :else
                     (throw (ex-info (str "unknown realm: " ~subject-name)
                                     {::unknown-realm ~subject-name}))))
            (let [missing (keys realm-predicates)]
              (throw (ex-info (str "missing realm cases: " (string/join ", " missing))
                              {::form &form ::missing-cases missing})))))

        (let [[?predicate-name ?exp] (first pairs)]
          (case ?predicate-name
            :else
            (if (empty? (rest pairs))
              `(let [~subject-name ~?subject]
                 (cond ~@(persistent! cond-rest)
                       :else ~?exp))
              (throw (ex-info ":else clause must be last" {:form &form})))

            (if-let [?predicate (get realm-predicates ?predicate-name)]
              (recur (rest pairs)
                (dissoc! realm-predicates ?predicate-name)
                (conj! (conj! cond-rest `(~?predicate ~subject-name)) ?exp))
              (throw (ex-info (str "unknown realm case: " ?predicate-name) {::form &form})))))))))
              
(defn shallow-predicate
  "Return a shallow predicate for the realm.

  This does not perform a full check whether a value conforms to a realm,
  but is rather intended only to figure out which one of a set of realms
  a value belongs to, and quickly.

  In particular, the predicate does not consider the realms of the
  contents of collections and records.

  It does, however, distinguish tuples of different sizes."
  [realm]
  (dispatch
      realm
    builtin-scalar?
    (case (builtin-scalar-realm-id realm)
      (:int) int?
      (:bigdec) (fn [x] (core/instance? java.math.BigDecimal x))
      (:float) float?
      (:double) double?
      (:keyword) keyword?
      (:symbol) symbol?
      (:string) string?
      (:boolean) boolean?
      (:any) any?
      (throw (Exception. (str "unknown builtin scalar realm: " (builtin-scalar-realm-id realm)))))

    predicate? (predicate-realm-predicate realm)
    
    optional?
    (let [inner-predicate (shallow-predicate (optional-realm-realm realm))]
      (fn [x]
        (or (nil? x)
            (inner-predicate x))))
    
    integer-from-to?
    (let [from (integer-from-to-realm-from realm)
          to (integer-from-to-realm-to realm)]
      (fn [x]
        (and (integer? x)
             (>= x from)
             (<= x to))))
    
    union?
    (let [predicates (map shallow-predicate (union-realm-realms realm))]
      (fn [x]
        (clojure.core/boolean (some #(% x) predicates))))
    
    intersection?
    (let [predicates (map shallow-predicate (intersection-realm-realms realm))]
      (fn [x]
        (every? #(% x) predicates)))
    
    enum? (enum-realm-values realm)
    sequence-of? sequential?
    set-of? set?
    map-with-keys? map?
    map-of? map?

    tuple?
    (let [size (count (tuple-realm-realms realm))]
      (fn [x]
        (and (sequential? x)
             (= (count x) size))))

    record? (record-realm-predicate realm)

    function? fn?

    delayed? (shallow-predicate (force (delayed-realm-delay realm)))

    protocol?
    (fn [thing] (clojure.core/satisfies? (protocol-realm-protocol realm) thing))

    named?
    (shallow-predicate (named-realm-realm realm))

    restricted? ; debatable
    (let [predicate (restricted-realm-predicate realm)
          realm-predicate (shallow-predicate (restricted-realm-realm realm))]
      (fn [thing]
        (and (realm-predicate thing)
             (predicate thing))))))
