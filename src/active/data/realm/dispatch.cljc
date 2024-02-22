(ns active.data.realm.dispatch
  (:require [active.data.realm :as realm]
            [clojure.string :as string]))

(defmacro union-case
  "(dispatch union-realm subject & cases)

where union-realm is a statically resolvable union realm,
  
and where each case has one of the following forms:

realm expr
:else expr

The realm must be the name of one of the components of union-realm.

This will generate a dispatch on subject according to realm type,
where each expr fires when subject belongs to the corresponding realm.

If there is no explicit :else clause, this will throw an exception on
non-realm values.

This will throw compile-time errors if there are unknown or uncovered
realm cases."
  [?union-realm ?subject & ?clauses]
  (let [union-realm (eval ?union-realm)
        realms (realm/union-realm-realms union-realm)
        pairs (partition 2 ?clauses)
        subject-name `subject#]
    (loop [pairs pairs
           realms (transient (set realms))
           cond-rest (transient [])]
      (if (empty? pairs)
        (let [realms (persistent! realms)]
          (if (empty? realms)
            `(let [~subject-name ~?subject]
               (cond ~@(persistent! cond-rest)
                     :else
                     (throw (ex-info (str "unknown realm: " ~subject-name)
                                     {::unknown-realm ~subject-name}))))
            (let [missing (map realm/description realms)]
              (throw (ex-info (str "missing realm cases: " (string/join ", " missing))
                              {::form &form ::missing-cases missing})))))

        (let [[?realm-name ?exp] (first pairs)]
          (case ?realm-name
            :else
            (if (empty? (rest pairs))
              `(let [~subject-name ~?subject]
                 (cond ~@(persistent! cond-rest)
                       :else ~?exp))
              (throw (ex-info ":else clause must be last" {:form &form})))

            (let [realm (eval ?realm-name)]
              (if (contains? realms realm)
                (recur (rest pairs)
                       (disj! realms realm)
                       (conj! (conj! cond-rest `(realm/contains? ~?realm-name ~subject-name)) ?exp))
                (throw (ex-info (str "unknown realm case: " ?realm-name) {::form &form}))))))))))
