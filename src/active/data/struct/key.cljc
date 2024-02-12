(ns ^:no-doc active.data.struct.key
  (:refer-clojure :exclude (set)))

(defprotocol ^:private IKey
  (-set-optimized! [this opt-get opt-assoc] "Set (optimize) get and assoc functions. Note that these don't take the key: (get m) => value for key, and (assoc m value) => new m"))

(deftype ^:private Key [^clojure.lang.Symbol sym ^:unsynchronized-mutable opt-get ^:unsynchronized-mutable opt-assoc]
  IKey
  (-set-optimized! [this opt-get opt-assoc]
    (set! (.-opt-get this) opt-get)
    (set! (.-opt-assoc this) opt-assoc))
  
  #?@(:clj
      [clojure.lang.IHashEq
       (hasheq [this]
               (.hasheq sym))

       Object
       (hashCode [this]
                 (.hashCode sym))
       
       (equals [this other]
               (if (instance? Key other)
                 (.equals sym (.-sym ^Key other))
                 false))

       (toString [this]
                 (name sym))]

      :cljs
      [Object
       (toString [this]
                 (name sym))
       IEquiv
       (-equiv [this other]
               (if (instance? Key other)
                 (-equiv sym (.-sym ^Key other))
                 false))

       IHash
       (-hash [this]
              (hash sym))])

  #?@(:clj
      ;; Note: the struct-map implementation of get and assoc will use 'optimized-for' info
      [clojure.lang.IFn
       (invoke [this m]
               (if opt-get
                 (opt-get m)
                 (get m this)))
       (invoke [this m v]
               (if opt-assoc
                 (opt-assoc m v)
                 (assoc m this v)))]

      :cljs
      [IFn
       (-invoke [this m]
                (if opt-get
                  (opt-get m)
                  (get m this)))
       (-invoke [this m v]
                (if opt-assoc
                  (opt-assoc m v)
                  (assoc m this v)))]))

#?(:clj
   (defmethod print-method Key [sk ^java.io.Writer writer]
     (.write writer (name (.-sym sk))))

   :cljs
   (extend-protocol IPrintWithWriter
     Key
     (-pr-writer [sk writer opts]
       (-write writer (name (.-sym sk))))))

(defn make [sym]
  (Key. sym nil nil))

(defmacro def-key [name]
  `(def ~name (make (symbol ~(str *ns*) ~(str name)))))

(defn set-optimized! [^Key key opt-get opt-assoc]
  ;; Note: should only be called once and immediately after construction/during definition;
  ;; therefor it's ok to just use :unsynchronized-mutable fields.
  (-set-optimized! key opt-get opt-assoc))
