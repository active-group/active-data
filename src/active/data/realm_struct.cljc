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

         (set-realm-struct-meta! ~t [~@pairs])

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

(defn ^:no-doc set-realm-struct-meta! [struct field-shortcut-pairs]
  (let [own-field-realms (map (fn [[field realm]]
                                [field (realm/compile realm)])
                              field-shortcut-pairs)
        all-field-realms-map (into {} (concat own-field-realms
                                              (when-let [ext (closed-struct/extended-struct struct)]
                                                (get (meta ext) realm-struct-meta/fields-realm-map-meta-key))))]
    (closed-struct/alter-meta!
     struct assoc
     ;; Note: record-realm don't have inheritance, which is why we need to pass all field realms to it;
     ;; validators of extended structs are already considered by struct-maps; thus we only need to validate our own fields:
     closed-struct-meta/validator-meta-key (validator own-field-realms)
     ;; Note: fields-realm-map is redundand - could be reconstructed from record-realm
     realm-struct-meta/fields-realm-map-meta-key all-field-realms-map
     realm-struct-meta/record-realm-meta-key (realm/create-realm-struct-realm struct all-field-realms-map))))
