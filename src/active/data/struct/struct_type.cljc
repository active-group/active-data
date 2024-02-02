(ns ^:no-doc active.data.struct.struct-type
  (:require [active.data.struct.struct-meta :as struct-meta])
  (:refer-clojure :exclude [alter-meta!
                            get-validator set-validator!
                            #?@(:cljs [contains? keys])]
                  :rename {contains? clj-contains?
                           keys clj-keys}))

(declare equals?)

(defprotocol IStructType
  (-variant [this])
  (-alter-meta! [this f args]))

(defprotocol IStructTypeVariant
  (-variant-name [this]
    ;; "Struct" or "Record" or a record's name? .. TODO or add full flexibility for printout of a struct-type?
    )
  (-construct-from [this struct-type m])
  (-identifier [this struct-type]
    ;; optional additional value for equality of struct-types
    )
  (-print-map-prefix [this struct-type]
    ;; prefix for map printouts
    )
  (-locked-maps? [this] "If dissoc/assoc with other keys is an error, or transforms this into a hash-map otherwise.")
  )

(defn print-map-prefix [struct-type]
  (-print-map-prefix (-variant struct-type) struct-type))

(defn locked-maps? [struct-type]
  (-locked-maps? (-variant struct-type)))

(defn set-validator! [struct-type v]
  (-alter-meta! struct-type assoc [struct-meta/validator-meta-key v]))

(defn get-validator [struct-type]
  (get (meta struct-type) struct-meta/validator-meta-key))

(defn get-current-validator! [struct-type]
  ;; TODO: resolve 'dynamic' validators here
  (get-validator struct-type))

(defn- ctor [struct-type m]
  (-construct-from (-variant struct-type) struct-type m))

(deftype ^:private StructType [keys keyset index-map variant ^:unsynchronized-mutable _meta]
  ;; Note: 'keys' is in original order; keyset the same as a set. index-map {key => index in raw data}
  IStructType
  (-variant [this] (.-variant this))
  (-alter-meta! [this f args]
    (set! (.-_meta this) (apply f (.-_meta this) args))
    (.-_meta this))

  ;; TODO: variant identifier in hashing.
  #?@(:clj
      [clojure.lang.IHashEq
       (hasheq [this]
               (hash index-map))
       Object
       (equals [this other]
               (equals? this other))

       clojure.lang.IObj
       clojure.lang.IMeta
       (withMeta [this meta] (StructType. keys keyset index-map variant meta))
       (meta [this] _meta)

       clojure.lang.IFn
       (applyTo [this arglist] (if (= 1 (count arglist)) (ctor this (first arglist)) (ctor this (apply array-map arglist))))
       (invoke [this a] (ctor this a))
       (invoke [this] (ctor this {}))
       (invoke [this a b] (ctor this (array-map a b)))
       (invoke [this a b c d] (ctor this (array-map a b c d)))
       (invoke [this a b c d e f] (ctor this (array-map a b c d e f)))
       (invoke [this a b c d e f g h] (ctor this (array-map a b c d e f g h)))
       (invoke [this a b c d e f g h i j] (ctor this (array-map a b c d e f g h i j)))
       (invoke [this a b c d e f g h i j k l] (ctor this (array-map a b c d e f g h i j k l)))
       (invoke [this a b c d e f g h i j k l m n] (ctor this (array-map a b c d e f g h i j k l m n)))
       (invoke [this a b c d e f g h i j k l m n o p] (ctor this (array-map a b c d e f g h i j k l m n o p)))
       (invoke [this a b c d e f g h i j k l m n o p q r] (ctor this (array-map a b c d e f g h i j k l m n o p q r)))
       (invoke [this a b c d e f g h i j k l m n o p q r s t] (ctor this (array-map a b c d e f g h i j k l m n o p q r s t)))
       ]

      :cljs
      [Object
       (equiv [this other] (-equiv this other))

       IWithMeta
       (-with-meta [this meta] (StructType. keys keyset index-map ctor ctor-m meta))
       IMeta
       (-meta [this] _meta)

       IEquiv
       (-equiv [this other]
               (equals? this other))

       ;; TODO
       ;;IHash
       ;;(-hash [this] (caching-hash this hash-unordered-coll _hash))

       IFn
       (-invoke [this a] (ctor this a))
       (-invoke [this] (ctor this {}))
       (-invoke [this a b] (ctor this (array-map a b)))
       (-invoke [this a b c d] (ctor this (array-map a b c d)))
       (-invoke [this a b c d e f] (ctor this (array-map a b c d e f)))
       (-invoke [this a b c d e f g h] (ctor this (array-map a b c d e f g h)))
       (-invoke [this a b c d e f g h i j] (ctor this (array-map a b c d e f g h i j)))
       (-invoke [this a b c d e f g h i j k l] (ctor this (array-map a b c d e f g h i j k l)))
       (-invoke [this a b c d e f g h i j k l m n] (ctor this (array-map a b c d e f g h i j k l m n)))
       (-invoke [this a b c d e f g h i j k l m n o p] (ctor this (array-map a b c d e f g h i j k l m n o p)))
       (-invoke [this a b c d e f g h i j k l m n o p q r] (ctor this (array-map a b c d e f g h i j k l m n o p q r)))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t] (ctor this (array-map a b c d e f g h i j k l m n o p q r s t)))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest] (ctor this (apply array-map a b c d e f g h i j k l m n o p q r s t rest)))
       ]))

(defn- equals? [^StructType struct other]
  (if (instance? StructType other)
    ;; Note: this makes sure keys are in the same order (required because of the internal representation as an array)
    (let [other ^StructType other]
      (and (= (.-variant struct) (.-variant other))
           (= (-identifier (.-variant struct) struct) (-identifier (.-variant other) other))
           ;; OPT: maybe faster to compare only the key vector?
           (= (.-index-map struct) (.-index-map other))))
    false))

(defn alter-meta! [^StructType struct f & args]
  ;; we want to add meta-data that references the struct itself. Needs mutability therefor.
  (-alter-meta! struct f args))

#?(:clj
   ;; TODO: tune that a bit, add namespace, deduplicate impls.
   
   (defmethod print-method StructType [^StructType s ^java.io.Writer writer]
     (.write writer "#")
     (.write writer (-variant-name (.-variant s)))
     (.write writer "{")
     (when-let [k (first (.-keys s))]
       (print-method k writer))
     (doseq [k (rest (.-keys s))]
       (.write writer ", ")
       (print-method k writer))
     (.write writer "}"))

   :cljs
   (extend-protocol IPrintWithWriter
     StructType
     (-pr-writer [^StructType s writer _]
       (write-all writer "#")
       (write-all writer (-variant-name (.-variant s)))
       (write-all writer "{")
       (doseq [k (interpose ", " (.-keys s))]
         (write-all writer k))
       (write-all writer "}"))))

(defn struct-type? [v]
  (instance? StructType v))

(defn create ^StructType [fields variant meta]
  (StructType. (vec fields)
               (set fields)
               (loop [idx 0
                      r (transient {})
                      fs fields]
                 (if-not (empty? fs)
                   (recur (inc idx)
                          (assoc! r (first fs) idx)
                          (rest fs))
                   (persistent! r)))
               variant
               meta))

(defn size [^StructType t]
  (count (.-keys t)))

(defn contains? [^StructType t key]
  (clj-contains? (.-index-map t) key))

(defn keys "Vector of keys in order given in [[create]]." [^StructType t]
  (.-keys t))

(defn keyset [^StructType t]
  (.-keyset t))

(defn maybe-index-of [^StructType t key default]
  (get (.-index-map t) key default))

#_(let [not-found #?(:clj (Object.) :cljs #js {})] ;; TODO: remove?
  (defn index-of [t key]
    (let [idx (maybe-index-of t key not-found)]
      (if (identical? idx not-found)
        (throw (ex-info "Key not in struct" {:key key
                                             :available (.-keys t)}))
        idx))))

(defn is-index-of? [t key index]
  (= key (get (keys t) index)))
