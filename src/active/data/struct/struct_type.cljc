(ns ^:no-doc active.data.struct.struct-type
  (:refer-clojure :exclude [alter-meta!
                            get-validator set-validator!
                            #?@(:cljs [contains? keys])]
                  :rename {contains? clj-contains?
                           keys clj-keys}))

(declare equals?)
(declare calc-hash)

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

(defn variant [struct-type]
  (-variant struct-type))

(defn print-map-prefix [struct-type]
  (-print-map-prefix (-variant struct-type) struct-type))

(defn locked-maps? [struct-type]
  (-locked-maps? (-variant struct-type)))

(defn identifier [struct-type]
  (-identifier (-variant struct-type) struct-type))

(defn- ctor [struct-type m]
  (-construct-from (-variant struct-type) struct-type m))

(deftype ^:private StructType [keys keyset index-map variant validator ^:unsynchronized-mutable _meta]
  ;; Note: 'keys' is in original order; keyset the same as a set. index-map {key => index in raw data}
  IStructType
  (-variant [this] (.-variant this))
  (-alter-meta! [this f args]
    (set! (.-_meta this) (apply f (.-_meta this) args))
    (.-_meta this))

  #?@(:clj
      [clojure.lang.IHashEq
       (hasheq [this]
               (calc-hash this))
       Object
       (equals [this other]
               (equals? this other))

       clojure.lang.IObj
       clojure.lang.IMeta
       (withMeta [this meta] (StructType. keys keyset index-map variant validator meta))
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
       (-with-meta [this meta] (StructType. keys keyset index-map variant validator meta))
       IMeta
       (-meta [this] _meta)

       IEquiv
       (-equiv [this other]
               (equals? this other))

       IHash
       (-hash [this] (calc-hash this))

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

(defn get-validator [^StructType struct-type]
  (.-validator struct-type))

(defn get-current-validator! [struct-type]
  ;; TODO: resolve 'dynamic' validators here
  (get-validator struct-type))

(defn- calc-hash [^StructType struct]
  (+ (let [id (identifier struct)]
       (if (some? id)
         (hash id)
         0))
     (hash (.-keys struct))))

(defn- equals? [^StructType struct other]
  (if (instance? StructType other)
    ;; Note: this makes sure keys are in the same order (required because of the internal representation as an array)
    (let [other ^StructType other]
      ;; Note: different variants with same identifier are equal - but that's more like an optimization; not a feature.
      (and (= (-identifier (.-variant struct) struct) (-identifier (.-variant other) other))
           (= (.-keys struct) (.-keys other))))
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
     ;; TODO: use print-prefix-map, print-map?
     (-pr-writer [^StructType s writer opts]
       (pr-writer "#" writer opts)
       (pr-writer (-variant-name (.-variant s)) writer opts)
       (pr-writer "{" writer opts)
       (doseq [k (interpose ", " (.-keys s))]
         (pr-writer k writer opts))
       (pr-writer "}" writer opts))))

(defn struct-type? [v]
  (instance? StructType v))

(defn create ^StructType [fields variant validator meta]
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
               validator
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
