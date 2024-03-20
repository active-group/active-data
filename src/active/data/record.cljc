(ns active.data.record
  (:require [active.data.raw-record :as raw-record #?@(:cljs [:include-macros true])]
            [active.data.realm.internal.record-meta :as realm-record-meta]
            [active.data.struct.validator :as struct-validator]
            [active.data.struct.internal.struct-type :as struct-type]
            [active.data.realm :as realm]
            [active.data.realm.validation :as realm-validation]))

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
  Defines a record and its keys, given a realm for each key:
  
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
    (assert (every? #{:extends} (map first options)) "Invalid option")
    `(do (raw-record/def-record ~t
           ~@(apply concat options)
           :validator (validator (map (fn [[field# realm#]]
                                           [field# (if realm# (realm/compile realm#) realm/any)])
                                      [~@pairs])
                                 ~(:extends options))
           [~@fields])

         (set-realm-record-meta! ~t (map (fn [[field# realm#]]
                                           [field# (if realm# (realm/compile realm#) realm/any)])
                                         [~@pairs]))

         ~t)))

(defn ^:no-doc validator [field-realm-pairs opt-extended-record]
  (-> (struct-validator/field-validators
       (into {}
             (map (fn [[field realm]]
                    (let [validator (realm-validation/validator realm)]
                      [field validator]))
                  (concat (when opt-extended-record
                            (when-let [ext-fields-realm-map (get (meta opt-extended-record) realm-record-meta/fields-realm-map-meta-key)]
                              ext-fields-realm-map))
                          field-realm-pairs))))
      (struct-validator/conditionally realm-validation/checking?)))

(defn ^:no-doc set-realm-record-meta! [t own-field-realm-pairs]
  ;; if record extends a realm-record, add the realms of inherited keys
  (let [all-field-realms-map (into {} (concat own-field-realm-pairs
                                              (when-let [ext (raw-record/record-extends t)]
                                                (get (meta ext) realm-record-meta/fields-realm-map-meta-key))))]
    (struct-type/alter-meta!
     t assoc
     realm-record-meta/fields-realm-map-meta-key all-field-realms-map
     realm-record-meta/record-realm-meta-key (realm/create-realm-record-realm t all-field-realms-map))))
