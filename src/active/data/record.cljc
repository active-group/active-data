(ns active.data.record
  (:require [active.data.struct :as struct #?@(:cljs [:include-macros true])]
            [active.data.struct.struct-type :as struct-type]
            [active.data.struct.struct-meta :as struct-meta])
  (:refer-clojure :exclude [record?]))

(declare to-record-map record?)

(defrecord ^:no-doc RecordVariant [print-prefix]
  struct-type/IStructTypeVariant
    (-variant-name [this] "active.data.record.Record")
    (-construct-from [this struct-type m]
      (to-record-map struct-type m))
    (-identifier [this struct-type]
      struct-type)
    (-print-map-prefix [this struct-type]
      print-prefix)
    (-locked-maps? [this] true))

(defn ^:no-doc record-variant [var]
  (RecordVariant. (str "#" (symbol var))))

(defmacro def-record
  [t & args]
  (let [[extends _meta fields] (struct/parse-def-struct-args args)]
    `(struct/def-struct-type* ~t ~extends ~_meta ~fields (record-variant (var ~t)))))

(defn ^:no-doc to-record-map [t m]
  ;; Note: 'private' - use record as fn to construct
  (assert (record? t))
  (assert (= (struct-type/keyset t) (set (map first m))))
  (struct/to-struct-map t m))

;; TODO
#_(set-validator   (combined extended-validator this-validator))

(defn record-of
  "Returns the record defined via [[def-record]] for the given instance of it."
  [m]
  (struct/struct-of m))

(defn record-keys
  "Returns the keys of a record as a set, including those added via extension.

  Note: use [[keys]] to get the keys of an instance of a particular record instead."
  [t]
  (assert (record? t))
  (struct-type/keyset t))

(defn record?
  "Tests if `v` is a record, defined via [[def-record]].

  Note: use [[is-a?]] to test for instances of a particular record instead."
  [v]
  (and (struct/struct? v)
       (instance? RecordVariant (struct-type/-variant v))))

(defn- is-exactly-a?-0 [t m]
  (= t (struct/struct-of m)))

(defn is-exactly-a?
  "Tests if `v` is an instance of the given record `t`."
  [t v]
  (assert (record? t))
  (and (struct/struct-map? v)
       (= t (struct/struct-of v))))

(defn- is-extension-of?-0 [parent-t child-t]
  (when-let [et (get (meta child-t) struct-meta/extends-meta-key)]
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
