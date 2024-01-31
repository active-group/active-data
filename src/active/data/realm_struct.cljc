(ns active.data.realm-struct
  (:require [active.data.struct :as struct #?@(:cljs [:include-macros true])]
            [active.data.struct.closed-struct :as closed-struct]
            [active.data.struct.closed-struct-meta :as closed-struct-meta]
            [active.data.realm.realm-struct-meta :as realm-struct-meta]
            [active.data.struct.validator :as struct-validator]
            [active.data.realm :as realm]
            [active.data.realm.validation :as realm-validation]))

(defmacro def-realm-struct
  "
  ```
  Defines a struct and its keys, given a realm for each key:
  
  (def-realm-struct T
    [f1 realm/int
     f2 realm/string])

  A corresponding struct map can be created by using the struct as a function:

  ```
  (T f1 42 f2 \"foo\")

  (T {f1 42 f2 \"foo\"})
  ```
  "
  [t & args]
  ;; Note: our 'fields' are different than def-struct's, but 'parse-def-struct-args' ignores that.
  (let [[extends _meta fields*] (struct/parse-def-struct-args args)
        pairs (map vec (partition 2 fields*))
        fields (map first pairs)]
    `(do (struct/def-struct* ~t ~extends
           ~_meta
           [~@fields])

         ;; FIXME: include extended fields:
         (let [field-realms# (map (fn [[field# realm#]]
                                    [field# (realm/compile realm#)])
                                  [~@pairs])]
           (closed-struct/alter-meta!
            ~t assoc
            closed-struct-meta/validator-meta-key (validator field-realms#)
            realm-struct-meta/record-realm-meta-key (realm/create-realm-struct-realm ~t field-realms#)))

         ~t)))

(defrecord ^:private RealmStructValidator [validators]
  struct-validator/IMapValidator
  (-validate-field! [this field value]
    (when (realm-validation/checking?) ; FIXME slow, shouldn't check for each field
      ((get validators field) value)))

  (-validate-map! [this m] nil))

(defn ^:no-doc validator [field-realm-pairs]
  (RealmStructValidator.
   (into {}
         (map (fn [[field realm]]
                [field (realm-validation/validator realm)])
              field-realm-pairs))))
