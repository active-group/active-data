(ns active.data.record
  "Records distinctly define compound data.

  ```
  (def-record Person [person-name person-age])
  ```

  Instances of the record can be created using the record as a function:

  ```
  (def p1 (Person person-name \"Hugo\" person-age 27))

  (def p2 (Person {person-name \"Tina\" person-age 31}))
  ```

  Access and modification can be done using the record keys as a function:

  ```
  (= 27 (person-age p1))

  (= 45 (person-age (person-age p2 45)))
  ```

  Values can be tested to see if they are instances of a record

  ```
  (is-a? Person p1)

  (not (is-a? Person :foo))
  ```

  Record instances support the standard map functions, using the defined keys:

  ```
  (update p1 person-age inc)
  ```

  As well as destructuring like maps:

  ```
  (let [{p1-age person-age, p1-name person-name} p1]
    ...)
  ```

  But trying to add keys not defined in the record, or removing any
  keys will result in an error!

  ```
  (assoc p1 :foo :bar) ;; -> error

  (dissoc p1 person-age) ;; -> error
  ```

  Record instances have a transient variant. Use the standard [[assoc!]] to modify them:

  ```
  (persistent! (assoc! (transient p1) person-name \"Sam\"))
  ```

  Records support some reflection at runtime:

  ```
  (= Person (record-of p1))

  (= [person-name person-age] (record-keys Person))

  (= 'user/Person (record-name Person))

  (record? Person)

  Unlike clojures [[defrecord]], these records don't define a new type
  in the host language. That has the disadvantage that you cannot
  implement protocols for these records, but has the advantage that
  records are first class values and are non-generative. That means
  redefining a record (with the same keys) still defines the same
  record and keys, that still work on instances of the previous
  definition.
  ```
  "
  (:require [active.data.raw-record :as raw-record #?@(:cljs [:include-macros true])]
            [active.data.realm.internal.record-meta :as realm-record-meta]
            [active.data.struct.validator :as struct-validator]
            [active.data.struct.internal.struct-type :as struct-type]
            [active.data.internal.export #?@(:clj [:refer [re-export]] :cljs [:refer-macros [re-export]])]
            [active.data.realm :as realm]
            [active.data.realm.validation :as realm-validation])
  (:refer-clojure :exclude [record? accessor]))

;; re-export reflection api
(re-export raw-record/record-of
           raw-record/record-name
           raw-record/record-extends
           raw-record/record?
           raw-record/record-keys)

;; re-export value predicates
(re-export raw-record/is-exactly-a?
           raw-record/is-extended-from?
           raw-record/is-a?)

;; re-export access/construction utils
(re-export raw-record/constructor
           raw-record/accessor
           raw-record/mutator
           raw-record/mutator!)

(defn ^:no-doc parse-fields [fields]
  (loop [fields fields
         res []]
    (if (empty? fields)
      res
      (let [field (first fields)]
        (if (= :- (second fields))
          (recur (drop 3 fields)
                 (conj res [field (second (rest fields))]))
          (recur (rest fields)
                 (conj res [field nil])))))))

(defmacro def-record
  "
  ```
  Defines a record and its keys, optionally giving a realm for each key:
  
  (def-record T
    [f1 :- realm/int,
     f2 :- realm/string])

  A corresponding instance can be created by using the record as a function:

  ```
  (T f1 42 f2 \"foo\")

  (T {f1 42 f2 \"foo\"})  
  ```

  Fields can be accessed and modified using the keys as functions:

  ```
  (= 42 (f1 (T f1 42 f2 \"foo\")))

  (= (T f1 42 f2 \"bar\") (f2 (T f1 42 f2 \"foo\") \"bar\"))
  ```
  "
  [t & args]
  (let [[options fields*] (raw-record/parse-def-record-args args)
        pairs (parse-fields fields*)
        fields (map first pairs)]
    (assert (every? #{:extends :validator} (map first options)) "Invalid option")
    ;; TODO: a usecase for a custom validator would be to define some additional invariants between fields; but if they also have realms,
    ;; then just the :validator option that replaces the default is very inconvenient; maybe add :add-validator option?
    `(do (raw-record/def-record ~t
           ~@(apply concat
                    (-> options
                        (update :validator
                                (fn [v]
                                  (if (contains? options :validator)
                                    ;; keep, even if specified as nil.
                                    v
                                    ;; default code otherwise.
                                    `(realm-validator (map (fn [[field# realm#]]
                                                             [field# (if realm# (realm/compile realm#) realm/any)])
                                                           [~@pairs])
                                                      ~(:extends options)))))))
           [~@fields])

         (set-realm-record-meta! ~t (map (fn [[field# realm#]]
                                           [field# (if realm# (realm/compile realm#) realm/any)])
                                         [~@pairs]))

         ~t)))

(defn- validate-only-if-checking [v] ;; -> name? move to realm-validation?
  (struct-validator/conditionally v realm-validation/checking?))

(defn ^:no-doc realm-validator
  ([field-realm-pairs]
   (realm-validator field-realm-pairs nil))
  ([field-realm-pairs opt-extended-record]
   (when-let [f-validators (->> (concat (when opt-extended-record
                                          (when-let [ext-fields-realm-map (get (meta opt-extended-record) realm-record-meta/fields-realm-map-meta-key)]
                                            ext-fields-realm-map))
                                        field-realm-pairs)
                                (map (fn [[field realm]]
                                       (when-not (= realm/any realm)
                                         (let [validator (realm-validation/validator realm)]
                                           [field validator]))))
                                (remove nil?)
                                (into {})
                                (not-empty))]
     (-> (struct-validator/field-validators f-validators)
         (validate-only-if-checking)))))

(defn ^:no-doc set-realm-record-meta! [t own-field-realm-pairs]
  ;; if record extends a realm-record, add the realms of inherited keys
  (let [all-field-realms-map (into {} (concat own-field-realm-pairs
                                              (when-let [ext (raw-record/record-extends t)]
                                                (get (meta ext) realm-record-meta/fields-realm-map-meta-key))))]
    (struct-type/alter-meta!
     t assoc
     realm-record-meta/fields-realm-map-meta-key all-field-realms-map
     realm-record-meta/record-realm-meta-key (realm/create-realm-record-realm t all-field-realms-map))))
