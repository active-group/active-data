(ns active.data.realm-record
  (:require [active.data.record :as record #?@(:cljs [:include-macros true])]
            [active.data.realm.realm-record-meta :as realm-record-meta]
            [active.data.struct.validator :as struct-validator]
            [active.data.struct.struct-type :as struct-type]
            [active.data.realm :as realm]
            [active.data.realm.validation :as realm-validation]))

(defmacro def-realm-record
  "
  ```
  Defines a record and its keys, given a realm for each key:
  
  (def-realm-record T
    [f1 realm/int
     f2 realm/string])

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
  (let [[options fields*] (record/parse-def-record-args args)
        pairs (map vec (partition 2 fields*))
        fields (map first pairs)]
    `(do (record/def-record ~t
           ~@(apply concat options)
           ;; TODO: validate extended fields too
           :validator (validator (map (fn [[field# realm#]]
                                           [field# (realm/compile realm#)])
                                         [~@pairs]))
           [~@fields])

         (set-realm-record-meta! ~t (map (fn [[field# realm#]]
                                           [field# (realm/compile realm#)])
                                         [~@pairs]))

         ~t)))

(defn ^:no-doc validator [field-realm-pairs]
  (struct-validator/field-validators
   (into {}
         (map (fn [[field realm]]
                (let [validator (realm-validation/validator realm)]
                  [field (fn [value]
                           (when (realm-validation/checking?) ; FIXME slow, shouldn't check for each field
                             (validator value)))]))
              field-realm-pairs))))

(defn ^:no-doc set-realm-record-meta! [t own-field-realm-pairs]
  ;; if record extends a realm-record, add the realms of inherited keys:x
  (let [all-field-realms-map (into {} (concat own-field-realm-pairs
                                              (when-let [ext (record/record-extends t)]
                                                (get (meta ext) realm-record-meta/fields-realm-map-meta-key))))]
    (struct-type/alter-meta!
     t assoc
     ;; Note: fields-realm-map is redundant - could be reconstructed from record-realm
     realm-record-meta/fields-realm-map-meta-key all-field-realms-map
     realm-record-meta/record-realm-meta-key (realm/create-realm-record-realm t all-field-realms-map))))
