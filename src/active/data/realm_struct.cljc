(ns active.data.realm-struct
  (:require [active.data.struct :as struct #?@(:cljs [:include-macros true])]
            [active.data.struct.closed-struct :as closed-struct]
            [active.data.struct.closed-struct-meta :as closed-struct-meta]
            [active.data.realm :as realm]
            [active.data.realm-struct.validator :as v]))

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
         (let [compiled-realms-map# (into {} (map (fn [[f# r#]]
                                                    [f# (realm/compile r#)])
                                                  [~@pairs]))]
           (closed-struct/alter-meta! ~t assoc
                                      closed-struct-meta/fields-realm-map-meta-key compiled-realms-map#
                                      closed-struct-meta/validator-meta-key (v/validator compiled-realms-map#)))
         
         ~t)))
