(ns ^{:doc "Forms for attaching realms to functions."}
  active.data.realm.attach
  (:refer-clojure :exclude [def defn fn])
  (:require [active.data.realm :as realm]
            [active.data.realm.schema :as realm-schema]
            [schema.core :as schema]
            #?(:clj [clojure.core :as core]
               :cljs [cljs.core :as core :include-macros true])))

;; TODO: sanitize options such as :always-validate

(def ^:no-doc fn-realm-meta-key ::realm)

(core/defn ^:no-doc parse-defn-args [next & rest-args]
  (let [[return-realm rest-args]
        (if (= :- next)
          [(first rest-args) (rest rest-args)]
          [nil rest-args])

        [doc-string rest-args]
        (if (string? (first rest-args))
          [(first rest-args) (rest rest-args)]
          [nil rest-args])

        [args-list & body] rest-args

        args-realms (loop [args-list args-list
                           res []]
                      (if (empty? args-list)
                        res
                        (let [arg (first args-list)]
                          (if (= :- (second args-list))
                            (recur (drop 3 args-list)
                                   (conj res [arg (second (rest args-list))]))
                            (recur (rest args-list)
                                   (conj res [arg nil]))))))]
    [return-realm doc-string args-realms body]))

(defmacro defn
  "Define a function with realms for return value and arguments.

  ```
  (defn myfun :- realm/integer [a :- realm/string]
    ...)
  ```
  "
  [name next & rest-args]
  ;; TODO: support some 'rest args' forms?
  (let [[return-realm doc-string args-realms body] (apply parse-defn-args next rest-args)
        arrow '->]
    `(let [ret# (schema/defn ~name ~@(when return-realm `[:- (realm-schema/schema (realm/compile ~return-realm))])
                  ~(vec (apply concat
                               (map (core/fn [[parameter realm]]
                                      `[~parameter ~@(when realm `[:- (realm-schema/schema (realm/compile ~realm))])])
                                    args-realms)))
                  ~@body)]
       (alter-meta! (var ~name) assoc fn-realm-meta-key
                    (realm/function ~@(map (core/fn [[arg realm]]
                                             (or realm realm/any))
                                           args-realms)
                                    ~arrow (or ~return-realm realm/any)))
       ret#)))

#?(:clj
   ;; Note: ClojureScript does not allow to alter the metadata of a var.
   (core/defn fn-realm
     "Return the realm of a function defined via [[defn]] given its var."
     [var]
     (get (meta var) fn-realm-meta-key)))
