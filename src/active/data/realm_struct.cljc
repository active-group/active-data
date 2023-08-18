(ns active.data.realm-struct
  (:require [active.data.struct :as struct #?@(:cljs [:include-macros true])]
            [active.data.struct.closed-struct :as closed-struct]
            [active.data.struct.closed-struct-meta :as closed-struct-meta]
            [active.data.struct.validator :as struct-validator]
            [active.data.realm :as realm]
            [active.data.realm.validation :as realm-validation]))

(defmacro def-realm-struct
  "
  ```
  (def-realm-struct T
    [f1 realm/int
     f2 realm/string])
  "
  
  [t & args]
  ;; Note: our 'fields' are different than def-struct's, but 'parse-def-struct-args' ignored that.
  (let [[extends _meta fields*] (struct/parse-def-struct-args args)
        pairs (map vec (partition 2 fields*))
        fields (map first pairs)]
    `(do (struct/def-struct* ~t ~extends
           ~_meta
           [~@fields])
         (let [realms-map# (into {} (map (fn [[field# realm#]]
                                           [field# (realm/compile realm#)])
                                         [~@pairs]))]
           (closed-struct/alter-meta! ~t assoc
                                      closed-struct-meta/fields-realm-map-meta-key realms-map#
                                      closed-struct-meta/validator-meta-key (validator realms-map#)))
         
         ~t)))

(defrecord ^:private RealmStructValidator [validators]
  struct-validator/IMapValidator
  (-validate-field! [this field value]
    (when (realm-validation/checking?) ; FIXME slow, shouldn't check for each field
      ((get validators field) value)))

  (-validate-map! [this m] nil))

(defn validator [field-realm-pairs]
  (RealmStructValidator.
   (into {}
         (map (fn [[field realm]]
                [field (realm-validation/validator realm)])
              field-realm-pairs))))
