(ns active.data.record
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
  redefining a record (with the same keys) still define the same
  record and keys, that still work on instances of the previous
  definition.
  ```
  
  "
  
  (:require [active.data.struct.key :as key #?@(:cljs [:include-macros true])]
            [active.data.struct :as struct #?@(:cljs [:include-macros true])]
            [active.data.struct.struct-type :as struct-type])
  (:refer-clojure :exclude [record?]))

(declare to-record-map record?)

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
  (RecordVariant. name extends))

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
                               (record-variant (symbol (str *ns*) (str '~t)) e#)
                               ~(:validator options)
                               nil)))

       ~@(for [f# fields]
           `(key/optimize-for! ~f# ~t))

       ~t)))

(defn ^:no-doc to-record-map [t m]
  ;; Note: 'private' - use record as fn to construct
  (assert (record? t))
  (struct/to-struct-map t m))

(defn record-of
  "Returns the record defined via [[def-record]] for the given instance of it."
  [m]
  (struct/struct-of m))

(defn record-name [t]
  (.-record-name ^RecordVariant (struct-type/variant t)))

(defn record-extends [t]
  (.-extends ^RecordVariant (struct-type/variant t)))

(defn record?
  "Tests if `v` is a record, defined via [[def-record]].

  Note: use [[is-a?]] to test for instances of a particular record instead."
  [v]
  (and (struct-type/struct-type? v)
       (instance? RecordVariant (struct-type/variant v))))

(def constructor struct/constructor)

(defn record-keys
  "Returns the keys of a record as a vector, including those added via extension.

  Note: use [[keys]] to get the keys of an instance of a particular record instead."
  [t]
  (assert (record? t))
  (struct-type/keys t))

(defn- is-exactly-a?-0 [t m]
  (= t (struct/struct-of m)))

(defn is-exactly-a?
  "Tests if `v` is an instance of the given record `t`."
  [t v]
  (assert (record? t))
  (and (struct/struct-map? v)
       (= t (struct/struct-of v))))

(defn- is-extension-of?-0 [parent-t child-t]
  (when-let [et (.-extends ^RecordVariant (struct-type/variant child-t))]
    (or (= parent-t et)
        (is-extension-of?-0 parent-t et))))

(defn is-extended-from?
  "Tests if `v` is an instance of an extension of the given record `t`."
  [t v]
  (assert (record? t))
  (and (struct/struct-map? v)
       (is-extension-of?-0 t (struct/struct-of v))))

(defn is-a?
  "Tests if `v` is an instance of the given record `t`, or an extension of it."
  [t v]
  (assert (record? t))
  (and (struct/struct-map? v)
       (or (is-exactly-a?-0 t v)
           (is-extension-of?-0 t (struct/struct-of v)))))