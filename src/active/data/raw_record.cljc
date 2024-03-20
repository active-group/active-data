(ns ^:no-doc active.data.raw-record
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

(defn record-name
  "Returns the name of the given record as a namespaced symbol."
  [t]
  (let [v (struct-type/variant t)]
    (assert (record-variant? v))
    (:record-name v)))

(defn record-extends
  "Returns the record that the given record `t` extends, or nil if it doesn't extend another record."
  [t]
  (let [v (struct-type/variant t)]
    (assert (record-variant? v))
    (:extends v)))

(defn record?
  "Tests if `v` is a record, as defined via [[def-record]].

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

(defn mutator!
  "Returns a slightly optimized mutator function for transient record instances.
  
  So instead of `(assoc! (transient (r r-key 42)) r-key v)` you can
  ```
  (def-record r [r-key])
  (def r-key! (mutator! r r-key))
  (r-key! (transient (r r-key 42)) 21)
  ```
  "
  [record key]
  (struct-map/mutator!* record key (struct-type-matcher record)))
