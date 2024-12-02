(ns ^{:doc
      "Namespace for constructing and using realms to describe the shape of data.
      
Also has [[contains?]] for dispatch on realms.

It also defines, via the [[compile]] function, a shorthand
notation for realms, that is available in most places a realm is expected,
i.e. in realm combinators and realm syntax."}
      active.data.realm
  (:refer-clojure :exclude [keyword symbol char boolean seq compile delay
                            contains? uuid])
  (:require
   #?(:clj [clojure.core :as core]
      :cljs [cljs.core :as core])
   #?(:clj [active.data.raw-record :refer [def-record is-a?]]
      :cljs [active.data.raw-record :refer [is-a?] :refer-macros [def-record]])
   [active.data.struct :as struct]
   [active.data.raw-record :as record]
   [active.data.realm.internal.record-meta :as realm-record-meta]
   [active.data.realm.internal.records :as realm-records]
   [clojure.string :as string]))

(defn contains?
  "Does a realm contain a value?
  
  Note this is intended for dispatch, not validation:

  It only does a shallow check that runs in constant time, and does
  not perform full validation."
  [realm x]
  ((realm-records/predicate realm) x))

(defn with-metadata
  "Return a new realm with added or changed metadata for one key."
  [realm key data]
  (assoc realm realm-records/metadata
         (assoc (realm-records/metadata realm) key data)))

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

#?(:clj (def rational
          (realm-records/builtin-scalar-realm
           realm-records/builtin-scalar-realm-id :rational
           realm-records/predicate rational?
           realm-records/description "rational" realm-records/metadata {})))
(def ^{:doc "Realm containing all numbers."}
  number
  (realm-records/builtin-scalar-realm
   realm-records/builtin-scalar-realm-id :number
   realm-records/predicate number?
   realm-records/description "number" realm-records/metadata {}))
(def ^{:doc "Realm containing all chars."}
  char
  (realm-records/builtin-scalar-realm
   realm-records/builtin-scalar-realm-id :char
   realm-records/predicate char?
   realm-records/description "char" realm-records/metadata {}))
(def ^{:doc "Realm containing all keywords."}
  keyword
  (realm-records/builtin-scalar-realm
   realm-records/builtin-scalar-realm-id :keyword
   realm-records/predicate keyword?
   realm-records/description "keyword" realm-records/metadata {}))
(def ^{:doc "Realm containing all symbols."}
  symbol
  (realm-records/builtin-scalar-realm
   realm-records/builtin-scalar-realm-id :symbol
   realm-records/predicate symbol?
   realm-records/description "symbol" realm-records/metadata {}))
(def ^{:doc "Realm containing all strings."}
  string
  (realm-records/builtin-scalar-realm
   realm-records/builtin-scalar-realm-id :string
   realm-records/predicate string?
   realm-records/description "string"  realm-records/metadata {}))
(def ^{:doc "Realm containing true and false."}
  boolean
  (realm-records/builtin-scalar-realm
   realm-records/builtin-scalar-realm-id :boolean
   realm-records/predicate boolean?
   realm-records/description "boolean" realm-records/metadata {}))
(def ^{:doc "Realm containing UUIDs."}
  uuid
  (realm-records/builtin-scalar-realm
   realm-records/builtin-scalar-realm-id :uuid
   realm-records/predicate uuid?
   realm-records/description "uuid" realm-records/metadata {}))
(def ^{:doc "Realm containing all Clojure values."}
  any
  (realm-records/builtin-scalar-realm
   realm-records/builtin-scalar-realm-id :any
   realm-records/predicate any?
   realm-records/description "any" realm-records/metadata {}))


(defn
  ^{:doc "Make a realm from a predicate and a prescription.

Don't use this if you don't have to, as the predicate is by its
nature opaque and not suitable for e.g. generation."}
  from-predicate
  [desc pred]
  (realm-records/from-predicate-realm realm-records/description desc
                                      realm-records/predicate pred
                                      realm-records/metadata {}))

(def ^{:doc "Realm containing all integers."}
  integer
  (realm-records/integer-from-to-realm realm-records/description "integer"
                                       realm-records/integer-from-to-realm-from nil
                                       realm-records/integer-from-to-realm-to nil
                                       realm-records/predicate integer?
                                       realm-records/metadata {}))

(defn ^{:doc "Create realm for integer range.

`from` and `to` are both inclusive."}
  integer-from-to
  [from to]
  (realm-records/integer-from-to-realm realm-records/description (str "integer from " from " to " to)
                                       realm-records/integer-from-to-realm-from from
                                       realm-records/integer-from-to-realm-to to
                                       realm-records/predicate (fn [x]
                                                                 (and (integer? x)
                                                                      (<= from x to)))
                                       realm-records/metadata {}))

(defn ^{:doc "Create a realm for integers above a lower bound.

`from` is inclusive."}
  integer-from
  [from]
  (realm-records/integer-from-to-realm realm-records/description (str "integer from " from)
                                       realm-records/integer-from-to-realm-from from
                                       realm-records/integer-from-to-realm-to nil
                                       realm-records/predicate (fn [x]
                                                                 (and (integer? x)
                                                                      (<= from x)))
                                       realm-records/metadata {}))

(defn ^{:doc "Create a realm for integers below an upper bound.

`to` is inclusive."}
  integer-to
  [to]
  (realm-records/integer-from-to-realm realm-records/description (str "integer to " to)
                                       realm-records/integer-from-to-realm-from nil
                                       realm-records/integer-from-to-realm-to to
                                       realm-records/predicate (fn [x]
                                                                 (and (integer? x)
                                                                      (<= x to)))
                                       realm-records/metadata {}))

(def ^{:doc "Realm containing all natural numbers.

I.e. all integers >= 0."}
  natural
  (realm-records/description (integer-from 0) "natural"))

(defn- make-real-range
  [clusive-left left right clusive-right]
  (realm-records/real-range-realm
   realm-records/description (if (and (nil? left) (nil? right))
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
   realm-records/real-range-realm-clusive-left clusive-left
   realm-records/real-range-realm-left left
   realm-records/real-range-realm-right right
   realm-records/real-range-realm-clusive-right clusive-right
   realm-records/predicate (cond
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
   realm-records/metadata {}))

(defn ^{:doc "Create a realm containg a real range.

With the four-argument version, the four arguments specify:

* `clusive-left` is one of the keywords `:in`, `:ex`, specifying
   whether the lower bound is inclusive or exclusive.
* `left` is the lower bound
* `right` is the upper bound
* `clusive-right` is one of the keywords `:in`, `:ex`, specifying
   whether the upper bound is inclusive or exclusive.

The two-argument version can be called as follows:

* `(real-range :in lower-bound)` for a real range with inclusive lower bound
* `(real-range :ex lower-bound)` for a real range with exclusive lower bound
* `(real-range upper-bound :in)` for a real range with inclusive upper bound
* `(real-range upper-bound :ex)` for a real range with exclusive upper bound"}
  real-range
  ([clusive-left left right clusive-right]
   (make-real-range clusive-left left right clusive-right))
  ([lr1 lr2]
   (case lr1
     (:in :ex) (make-real-range lr1 lr2 nil :ex)
     (case lr2
       (:in :ex) (make-real-range :ex nil lr1 lr2)))))

(def ^{:doc "Realm containing all real numbers."}
  real
  (real-range :in nil nil :in))

(defn- realm-seq-description
  [realms]
  (str "["
       (string/join ", " (map realm-records/description realms))
       "]"))

(declare compile)

(defn ^{:doc ""}
  optional
  [realm]
  (let [realm (compile realm)]
    (realm-records/optional-realm
     realm-records/description (str "optional " (realm-records/description realm))
     realm-records/predicate (let [inner-predicate (realm-records/predicate realm)]
                               (fn [x]
                                 (or (nil? x)
                                     (inner-predicate x))))
     realm-records/optional-realm-realm realm
     realm-records/metadata {})))

(defn union
  [& realms]
  (let [realms (map compile realms)]
    (realm-records/union-realm
     realm-records/description (str "union of " (realm-seq-description realms))
     realm-records/union-realm-realms realms
     realm-records/predicate (let [predicates (map realm-records/predicate realms)]
                               (fn [x]
                                 (core/boolean (some #(% x) predicates))))
     realm-records/metadata {})))

(defn enum
  "Creates a realm containing the values in `values`."
  [& values]
  (let [values-set (set values)]
    (realm-records/enum-realm
     realm-records/description (str "enumeration of [" (string/join ", " (map str values)) "]")
     realm-records/enum-realm-values values-set
     realm-records/predicate (fn [x]
                               (core/contains? values-set x))
     realm-records/metadata {})))

(defn intersection
  "Creates an intersection realm of its arguments.

  The returned realm contains the values that are contained
  by all the argument realms."
  ([realm] (compile realm))
  ([realm1 realm2] ; common case
   (let [realm1 (compile realm1)
         realm2 (compile realm2)]
     (realm-records/intersection-realm
      realm-records/description
      (if (is-a? realm-records/from-predicate-realm realm2)
        (str (realm-records/description realm1) " restricted to " (realm-records/description realm2))
        (str "intersection of [" (realm-records/description realm1) ", " (realm-records/description realm2) "]"))
      realm-records/intersection-realm-realms [realm1 realm2]
      realm-records/predicate (let [predicate1 (realm-records/predicate realm1)
                                    predicate2 (realm-records/predicate realm2)]
                                (fn [x]
                                  (and (predicate1 x)
                                       (predicate2 x))))
      realm-records/metadata {})))
  ([realm1 realm2 & realms-rest]
   (let [realms (map compile (list* realm1 realm2 realms-rest))]
     (realm-records/intersection-realm
      realm-records/description (str "intersection of " (realm-seq-description realms))
      realm-records/intersection-realm-realms realms
      realm-records/predicate (let [predicates (map realm-records/predicate realms)]
                                (fn [x]
                                  (every? #(% x) predicates)))
      realm-records/metadata {}))))

(defn sequence-of
  "Create realm containing uniform sequences.

  `realm` is the realm of the elements of the sequences."
  [realm]
  (let [realm (compile realm)]
    (realm-records/sequence-of-realm
     realm-records/description (str "sequence of " (realm-records/description realm))
     realm-records/sequence-of-realm-realm realm
     realm-records/predicate sequential?
     realm-records/metadata {})))

(defn set-of
  "Create realm containing uniform sets.

  `realm` is the realm of the elements of the sets."
  [realm]
  (let [realm (compile realm)]
    (realm-records/set-of-realm
     realm-records/description (str "set of " (realm-records/description realm))
     realm-records/set-of-realm-realm realm
     realm-records/predicate set?
     realm-records/metadata {})))

(defn map-with-keys
  "Create realm containing certain keys and a different realm for the value associated with each key.

  `keys-realm-map` is map from keys to the realms of their values."
  [keys-realm-map]
  (let [keys-realm-map (into {}
                             (map (fn [[key realm]]
                                    [key (compile realm)])
                                  keys-realm-map))]
    (realm-records/map-with-keys-realm
     realm-records/description (str "map with keys {"
                                    (string/join ", "
                                                 (core/map (fn [[key realm]]
                                                             (str key " -> " (realm-records/description realm)))
                                                           keys-realm-map))
                                    "}")
     realm-records/map-with-keys-realm-map keys-realm-map
     realm-records/predicate map?
     realm-records/metadata {})))

(defn map-of
  "Create realm containing uniform maps, where keys and value are each from a fixed realm.

  `key-realm` is the realm of the keys,
  `value-realm` is the realm of the values."
  [key-realm value-realm]
  (let [key-realm (compile key-realm)
        value-realm (compile value-realm)]
    (realm-records/map-of-realm
     realm-records/description (str "map of {"
                                    (realm-records/description key-realm)
                                    " -> "
                                    (realm-records/description value-realm)
                                    "}")
     realm-records/map-of-realm-key-realm key-realm
     realm-records/map-of-realm-value-realm value-realm
     realm-records/predicate map?
     realm-records/metadata {})))

(defn- map-with-tag-predicate [key value]
  (fn [x]
    (and (map? x)
         (= (get x key nil) value))))

(defn map-with-tag
  "Create realm for maps containing a tag.

  I.e. a certain `key` mapping to a certain `value`."
  [key value]
  (realm-records/map-with-tag-realm
   realm-records/description (str "map with tag " key " -> " value)
   realm-records/map-with-tag-realm-key key
   realm-records/map-with-tag-realm-value value
   realm-records/predicate (map-with-tag-predicate key value)
   realm-records/metadata {}))

(defn tuple
  "Create realm for tuples.

  These are sequences of a fixed length.
  `realms` is a sequence of the realms of the elements
  of the tuples."
  [& realms]
  (let [realms (mapv compile realms)]
    (realm-records/tuple-realm
     realm-records/description (str "tuple of ("
                                    (string/join ", " (map realm-records/description realms))
                                    ")")
     realm-records/tuple-realm-realms realms
     realm-records/predicate (let [size (count realms)]
                               (fn [x]
                                 (and (sequential? x)
                                      (= (count x) size))))
     realm-records/metadata {})))

(defn field
  "Create a record-field descriptor.

  * `name` is a symbol naming the field
  * `realm` is the realm for the field values
  * `getter` is the selector for that field"
  [name realm getter]
  (realm-records/record-realm-field realm-records/record-realm-field-name name
                                    realm-records/record-realm-field-realm (compile realm)
                                    realm-records/record-realm-field-getter getter))

(defn record
  "Create a record realm.

  * `name` is a symbol naming the record
  * `constructor` is a positional constructor
  * `pred` is a prediucate for the record
  * `fields` is a sequence of [[field]]s"
  [name constructor pred fields]
  (realm-records/record-realm
   realm-records/description (str "record " name
                                  " with fields "
                                  (string/join ", "
                                               (map (fn [field]
                                                      (str (realm-records/record-realm-field-name field) " from realm "
                                                           (realm-records/description (realm-records/record-realm-field-realm field))))
                                                    fields)))
   realm-records/record-realm-name name
   realm-records/record-realm-constructor constructor
   realm-records/predicate pred
   realm-records/record-realm-fields fields
   realm-records/metadata {}))

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
  "Returns a realm for a record from [[active.data.record]] or [[active.data.realm-record]]."
  [record]
  (or (get (meta record) realm-record-meta/record-realm-meta-key)
      (create-realm-record-realm record {})))

(defn- function-case-description
  [function-case]
  (str "function ("
       (string/join ", "
                    (map realm-records/description (realm-records/function-case-positional-argument-realms function-case)))
       (if-let [optional (realm-records/function-case-optional-arguments-realm function-case)]
         (str " & "
              (realm-records/description optional))
         "")
       
       ") -> "
       (realm-records/description (realm-records/function-case-return-realm function-case))))
   
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
                (realm-records/function-realm
                 realm-records/function-realm-cases
                 [(realm-records/function-case realm-records/function-case-positional-argument-realms positional#
                                               realm-records/function-case-optional-arguments-realm nil
                                               realm-records/function-case-return-realm return#)]))))
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
                  (realm-records/function-realm
                   realm-records/function-realm-cases
                   [(realm-records/function-case realm-records/function-case-positional-argument-realms positional#
                                                 realm-records/function-case-optional-arguments-realm optional#
                                                 realm-records/function-case-return-realm return#)])))))

          (recur (rest list)
                 (conj! positional `(compile ~f))))))))

(defmacro function
  "Shorthand for function realms with a single arity.

Here are the different forms:

  * `(function r1 r2 r3 -> r) for fixed arity
  * `(function r1 r2 r3 & (rs) -> r)` for rest arguments
  * `(function r1 r2 r3 & [rr1 rr2]) -> r)` for 2 optional args
  * `(function r1 r2 r3 & {:a ra :b rb :c rc} -> r)` for optional keyword args"
  [& shorthand]
  (compile-function-case-shorthand shorthand))

(defn function-cases
  "Create a realm for functions with several cases.

   Typically applied to the return values of `function`."
  [& cases]
  (let [cases (mapcat realm-records/function-realm-cases cases)]
    (realm-records/function-realm realm-records/function-realm-cases cases
                                  realm-records/metadata {}
                                  realm-records/predicate fn?
                                  realm-records/description
                                  (if (= (count cases) 1)
                                    (function-case-description (first cases))
                                    (str "function with cases " (string/join ", " (map function-case-description cases)))))))

(defmacro delay
  "Delay the evaluation of a realm.

  Its operand must be an expression evaluating to a realm.

  This is for generating recursive realms."
  [realm-expression]
  `(let [delay-object# (core/delay (compile ~realm-expression))]
     (realm-records/delayed-realm realm-records/delayed-realm-delay delay-object#
                                  realm-records/metadata {}
                                  realm-records/predicate (fn [x#]
                                                            ((realm-records/predicate (force delay-object#)) x#))
                                  realm-records/description "delayed realm")))

(defn named
  "Name a realm.

  Returns a realm that describes the same values as `realm`.
  The name intended is for printing in polymorphic realms."
  [name realm]
  (let [realm (compile realm)]
    (realm-records/named-realm realm-records/named-realm-name name
                               realm-records/named-realm-realm realm
                               realm-records/metadata {}
                               realm-records/predicate (realm-records/predicate realm)
                               realm-records/description (str "realm named " name ": " (realm-records/description realm)))))

(defn restricted
  "Restrict a realm with a predicate.

  The resulting realm contains only those values of the input
  realm for which `pred` (a unary function) returns a true value.

  Only use this if you have to, as it forfeits inspectability."  
  [realm pred predicate-description]
  (intersection realm
                (from-predicate predicate-description
                                pred)))

(defn compile
  "Compile a realm shorthand into a realm object.

  This is called by most realm constructors and macros that generate
  realms from source code.
  
  Shorthand notation includes:

  * A one-element vector containing a realm denotes a [[sequence-of]] that realm.
  * Any vector that does not have length 1 containing realms denotes a [[tuple]] of those realms.
  * A one-element set containing a realm denotes a [[set-of]] that realm.
  * A map containing one entry, whose key and value are both realms, denotes a [[map-of]] of those realms.
  * Any other map denotes a [[map-with-keys]] of its keys and value realms.
  * Record types from [[active.data.record]] or [[active.data.realm-record]] denote the corresponding record realm.
  * Struct types from [[active.data.struct]] denote the corresponding record realm."  
  [shorthand]
  (if (is-a? realm-records/realm shorthand)
    shorthand
    (cond
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

      :else
      (throw (ex-info (str "unknown realm shorthand: " (pr-str shorthand))
                      {::unknown-realm-shorthand shorthand})))))
