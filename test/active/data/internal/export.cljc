(ns active.data.internal.export)

(defn- re-export-1
  [var-name]
  (let [local-name (symbol (name var-name))]
    `(do
       (def ~local-name ~var-name)
       (alter-meta! (var ~local-name)
                    (constantly (assoc (meta (var ~var-name))
                                       :re-exporting (var ~var-name)))))))

(defmacro re-export
  "Re-exports a bunch of names, copying metadata"
  [& names]
  `(do ~@(map re-export-1 names)))

  
