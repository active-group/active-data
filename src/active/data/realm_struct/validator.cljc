(ns ^:no-doc active.data.realm-struct.validator
  (:require [active.data.struct.validator :as v]
            [active.data.realm :as realm]))

(def matches? (constantly true)) ;; TODO: realm/matches? or alike

(defrecord ^:private RealmStructValidator [realms]
  v/IMapValidator
  (-validate-field! [this field value]
    (when-not (matches? (get realms field) value)
      (throw (ex-info "Value does not match realm" {:field field
                                                    ;; etc.
                                                    :value value}))))

  (-validate-map! [this m] nil))

(defn validator [field-realm-pairs]
  (RealmStructValidator. (apply hash-map field-realm-pairs)))

