(ns ^:no-doc active.data.struct.key
  (:require [active.data.struct.struct-type :as struct-type])
  (:refer-clojure :exclude (set)))

(defprotocol ^:private IKey
  (-optimize-for! [this struct])
  (-optimized-for? [this struct] "Returns the index in struct or nil."))

(deftype ^:private Key [^clojure.lang.Symbol sym ^:unsynchronized-mutable index]
  IKey
  (-optimize-for! [this s]
    (if-let [idx (struct-type/maybe-index-of s this nil)]
      (set! (.-index this) idx)
      (throw (ex-info (str "Unknown key: " this) {:key this :struct s}))))
  (-optimized-for? [this s]
    (when (and index
               (struct-type/is-index-of? s this index))
      index))
  
  #?@(:clj
      [clojure.lang.IHashEq
       (hasheq [this]
               (.hasheq sym))

       ;; TODO: withMeta meta, clojure.lang.IObj
       ;; TODO? java.lang.Comparable

       Object
       (hashCode [this]
                 (.hashCode sym))
       
       (equals [this other]
               (if (instance? Key other)
                 (= sym (.-sym other))
                 false))

       (toString [this]
                 ;; this is (str key)
                 ;; TODO: with or without ~ ?
                 (name sym))])

  #?@(:clj
      ;; Note: the struct-map implementation of get and assoc will use 'optimized-for' info
      [clojure.lang.IFn
       (invoke [this m] (get m this))
       (invoke [this m v] (assoc m this v))]

      :cljs
      [IFn
       (-invoke [this m] (get m this))
       (-invoke [this m v] (assoc m this v))]))

#?(:clj
   ;; add ~ to the namespaced symbol, so that quasiquoting the output
   ;; is an expression yielding the value.
   
   (defmethod print-method Key [sk ^java.io.Writer writer]
     (.write writer (name (.-sym sk))))

   :cljs
   (extend-protocol IPrintWithWriter
     Key
     (-pr-writer [sk writer opts]
       (-write writer (name (.-sym sk))))))

(defn make [sym]
  (Key. sym nil))

(defmacro def-key [name]
  `(def ~name (make (symbol ~(str *ns*) ~(str name)))))

(defn optimize-for! [^Key key struct]
  (assert (instance? Key key))
  (assert (struct-type/struct-type? struct))
  ;; Note: should only be called once and immediately after construction/during definition;
  ;; therefor it's ok to just use :unsynchronized-mutable fields.
  (-optimize-for! key struct))

(defn optimized-for? [key struct]
  (and (instance? Key key)
       (-optimized-for? ^Key key struct)))
