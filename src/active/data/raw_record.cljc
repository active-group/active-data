(ns active.data.raw-record
  "Records distinctly define structured data.

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
  (:require [active.data.struct.internal.key :as key #?@(:cljs [:include-macros true])]
            [active.data.struct.internal.closed-struct-map :as struct-map]
            [active.data.struct.internal.struct-type :as struct-type])
  (:refer-clojure :exclude [record? accessor]))

(declare to-record-map)
(declare record?)

(defrecord ^:no-doc RecordVariant [record-name extends]
  struct-type/IStructTypeVariant
    (-variant-name [this] "active.data.record.Record")
    (-construct-from [this struct-type m]
      (to-record-map struct-type m))
    (-identifier [this struct-type]
      record-name)
    (-print-map-prefix [this struct-type]
      (str "#" record-name))
    (-locked-maps? [this] true))

(defn ^:no-doc record-variant [name extends]
  (assert (symbol? name))
  (assert (or (nil? extends) (record? extends)))
  (RecordVariant. name extends))

(defn- record-variant? [v]
  (instance? RecordVariant v))

(defn ^:no-doc parse-def-record-args [args]
  ;; TODO: not actually checking possible errors... use spec for that?
  (loop [args args
         options {}
         fields nil]
    (if (empty? args)
      [options fields]
      (cond
        (keyword? (first args))
        (recur (rest (rest args))
               (assoc options (first args) (second args))
               nil)
        
        :else
        (recur (rest args)
               options
               (first args))))))

(defmacro def-record
  [t & args]
  (let [[options fields] (parse-def-record-args args)]
    (assert (every? #{:extends :validator} (map first options)) "Invalid option")
    `(do
       ~@(for [f# fields]
           `(key/def-key ~(cond-> f#
                            (and (contains? (meta t) :private)
                                 (not (contains? (meta f#) :private)))
                            (vary-meta assoc :private (:private (meta t))))))


       (def ~t
         (let [e# ~(:extends options)]
           (struct-type/create (cond->> ~fields
                                 e# (concat (record-keys e#)))
                               (record-variant (symbol ~(str *ns*) (str '~t)) e#)
                               ~(:validator options)
                               nil)))

       ~@(for [f# fields]
           `(key/set-optimized! ~f# (accessor ~t ~f#) (mutator ~t ~f#)))

       ~t)))

(defn ^:no-doc to-record-map [t m]
  ;; Note: 'private' - use record as fn to construct
  (assert (record? t))
  (struct-map/from-coll t m))

#_(defn- record-map? [v]
  (and (struct-map/struct-map? m)
       (record-variant? (struct-type/variant (struct-map/struct-of-map m)))))

(defn record-of
  "Returns the record defined via [[def-record]] for the given instance of it."
  [m]
  (let [v (struct-map/struct-of-map m)]
    (assert (record? v))
    v))

(defn record-name [t]
  (let [v (struct-type/variant t)]
    (assert (record-variant? v))
    (:record-name v)))

(defn record-extends [t]
  (let [v (struct-type/variant t)]
    (assert (record-variant? v))
    (:extends v)))

(defn record?
  "Tests if `v` is a record, defined via [[def-record]].

  Note: use [[is-a?]] to test for instances of a particular record instead."
  [v]
  (and (struct-type/struct-type? v)
       (record-variant? (struct-type/variant v))))

(defn record-keys
  "Returns the keys of a record as a vector, including those added via extension.

  Note: use [[keys]] to get the keys of an instance of a particular record instead."
  [t]
  (assert (record? t))
  (struct-type/keys t))

(defn- is-exactly?-0 [t-1 t-2]
  (= t-1 t-2))

(defn- is-exactly?-2 [t-1 t-2]
  (identical? t-1 t-2))

(defn- is-exactly-a?-0 [t m]
  (is-exactly?-0 t (struct-map/struct-of-map m)))

(defn is-exactly-a?
  "Tests if `v` is an instance of the given record `t`."
  [t v]
  (assert (record? t))
  (and (struct-map/struct-map? v)
       (= t (struct-map/struct-of-map v))))

(defn- is-extension-of?-0 [parent-t child-t]
  ;; is the record child-t a direct or indirect extension of parent-t?
  (when-let [et (record-extends child-t)]
    (or (is-exactly?-0 parent-t et)
        (is-extension-of?-0 parent-t et))))

(defn- is-extension-of?-2 [parent-t child-t]
  ;; is the record child-t a direct or indirect extension of parent-t?
  (when-let [et (:extends (struct-type/variant child-t))]
    (or (is-exactly?-2 parent-t et)
        (is-extension-of?-2 parent-t et))))

(defn is-extended-from?
  "Tests if `v` is an instance of an extension of the given record `t`."
  [t v]
  (assert (record? t))
  (and (struct-map/struct-map? v)
       (is-extension-of?-0 t (struct-map/struct-of-map v))))

(defn is-a?
  "Tests if `v` is an instance of the given record `t`, or an extension of it."
  [t v]
  (assert (record? t))
  (and (struct-map/struct-map? v)
       (let [r (struct-map/struct-of-map v)]
         (or (is-exactly?-0 t r)
             (is-extension-of?-0 t r)))))

(def ^:no-doc constructor struct-map/positional-constructor)

(defn- struct-type-matcher [record]
  ;; Note: this enables optimized access to keys in extended records; extended
  ;; keys must be come first for this, so the indices are the same as in the parent record!
  (fn [struct-type]
    ;; This may return false negatives (e.g. after hot code reload) as an optimization for the normal case.
    (or (is-exactly?-2 record struct-type)
        (and (record? struct-type)
             (is-extension-of?-2 record struct-type)))))

(defn ^:no-doc accessor [record key]
  (struct-map/accessor* record key (struct-type-matcher record)))

(defn ^:no-doc mutator [record key]
  (struct-map/mutator* record key (struct-type-matcher record)))

(defn mutator! [record key]
  (struct-map/mutator!* record key (struct-type-matcher record)))
