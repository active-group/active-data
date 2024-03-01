(ns active.data.realm.attach
  (:refer-clojure :exclude [def defn fn])
  (:require [active.data.realm :as realm]
            [active.data.realm.schema :as realm-schema]
            [schema.core :as schema]
            #?(:clj [clojure.core :as core]
               :cljs [cljs.core :as core :include-macros true])))

;; TODO: sanitize options such as :always-validate

(def ^:no-doc fn-realm-meta-key ::realm)

(defmacro defn
  [return-realm name next & rest-args]
  (let [[doc-string rest-args]
        (if (string? next)
          [next rest-args]
          [nil (cons next rest-args)])
        args (partition 2 (first rest-args))
        body (rest rest-args)
        arrow '->]
    `(let [ret# (schema/defn ~name :- (realm-schema/schema (realm/compile ~return-realm))
                  ~(vec (apply concat
                               (map (core/fn [[parameter realm]]
                                      `[~parameter :- (realm-schema/schema (realm/compile ~realm))])
                                    args)))
                  ~@body)]
       (alter-meta! (var ~name) assoc fn-realm-meta-key
                    (realm/function ~@(map second args) ~arrow ~return-realm))
       ret#)))

#?(:clj
   ;; Note: ClojureScript does not allow to alter the metadata of a var.
   (core/defn fn-realm
     "Return the realm of a function defined via [[defn]] given its var."
     [var]
     (get (meta var) fn-realm-meta-key)))
