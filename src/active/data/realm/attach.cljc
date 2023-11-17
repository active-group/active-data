(ns active.data.realm.attach
  (:refer-clojure :exclude [def defn fn])
  (:require [clojure.core :as core]
            [active.data.realm :as realm]
            [active.data.realm.schema :as realm-schema]
            [schema.core :as schema]))

; TODO: sanitize options such as :always-validate
; TODO: attach realm as metadata to function name

(defmacro defn
  [return-realm name next & rest-args]
    (let [[doc-string rest-args]
          (if (string? next)
            [next rest-args]
            [nil (cons next rest-args)])
          args (partition 2 (first rest-args))
          body (rest rest-args)]
      `(schema/defn ~name :- (realm-schema/schema (realm/compile ~return-realm))
         ~(vec (apply concat
                      (map (core/fn [[parameter realm]]
                             `[~parameter :- (realm-schema/schema (realm/compile ~realm))])
                           args)))
         ~@body)))

