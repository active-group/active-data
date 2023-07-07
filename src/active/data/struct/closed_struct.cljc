(ns ^:no-doc active.data.struct.closed-struct
  (:refer-clojure :exclude [set-validator! get-validator alter-meta!
                            #?@(:cljs [contains? keys])]
                  :rename {contains? clj-contains?
                           keys clj-keys}))

(declare equals?)

(defprotocol IClosedStruct
  (-alter-meta! [this f args]))

(deftype ^:private ClosedStruct [keys keyset index-map ^:unsynchronized-mutable _meta extended-struct]
  ;; Note: 'keys' is in original order; keyset the same as a set.
  IClosedStruct
  (-alter-meta! [this f args]
    (set! (.-_meta this) (apply f (.-_meta this) args))
    (.-_meta this))
  
  ;; TODO: hashing.
  #?@(:clj
      [clojure.lang.IHashEq
       (hasheq [this]
               (hash index-map))
       Object
       (equals [this other]
               (equals? this other))

       clojure.lang.IObj
       clojure.lang.IMeta
       (withMeta [this meta] (ClosedStruct. keys keyset index-map meta extended-struct))
       (meta [this] _meta)]

      :cljs
      [Object
       (equiv [this other] (-equiv this other))

       IWithMeta
       (-with-meta [this meta] (ClosedStruct. keys keyset index-map meta extended-struct))
       IMeta
       (-meta [this] _meta)

       IEquiv
       (-equiv [this other]
               (equals? this other))

       ;;IHash
       ;;(-hash [this] (caching-hash this hash-unordered-coll _hash))
       ]))

(defn- equals? [^ClosedStruct struct other]
  (if (instance? ClosedStruct other)
    ;; Note: this makes sure keys are in the same order (required because of the internal representation as an array)
    ;; OPT: faster to compare only the keys?
    (= (.-index-map struct) (.-index-map ^ClosedStruct other))
    false))

(defn alter-meta! [^ClosedStruct struct f & args]
  ;; we want to add meta-data that references the struct itself. Needs mutability therefor.
  (-alter-meta! struct f args))

#?(:clj
   ;; TODO: tune that a bit, add namespace, deduplicate impls.
   
   (defmethod print-method ClosedStruct [^ClosedStruct s ^java.io.Writer writer]
     (.write writer "#Struct{")
     (when-let [k (first (.-keys s))]
       (print-method k writer))
     (doseq [k (rest (.-keys s))]
       (.write writer ", ")
       (print-method k writer))
     (.write writer "}"))

   :cljs
   (extend-protocol IPrintWithWriter
     ClosedStruct
     (-pr-writer [^ClosedStruct s writer _]
       (write-all writer "#Struct{")
       (doseq [k (interpose ", " (.-keys s))]
         (write-all writer k))
       (write-all writer "}"))))

(defn closed-struct? [v]
  (instance? ClosedStruct v))

(def validator-meta-key ::validator)

(defn set-validator! [^ClosedStruct t validator]
  (alter-meta! t assoc validator-meta-key validator))

(defn get-validator [^ClosedStruct t]
  ;; Note: validators are optional.
  (assert (closed-struct? t)) ;; TODO: nice exception?
  (get (meta t) validator-meta-key))

(defn extended-struct [^ClosedStruct t]
  (.-extended-struct t))

(defn create [fields ^ClosedStruct extended-struct meta]
  ;; Note: keys of the extended struct must come first! (for optimizations to work)
  (let [all-fields (vec (concat (when (some? extended-struct) (.-keys extended-struct)) fields))]
    (ClosedStruct. all-fields
                   (set all-fields)
                   (loop [idx 0
                          r (transient {})
                          fs all-fields]
                     (if-not (empty? fs)
                       (recur (inc idx)
                              (assoc! r (first fs) idx)
                              (rest fs))
                       (persistent! r)))
                   meta
                   extended-struct)))

(defn size [^ClosedStruct t]
  (count (.-keys t)))

(defn contains? [^ClosedStruct t key]
  (clj-contains? (.-index-map t) key))

(defn maybe-index-of [^ClosedStruct t key default]
  (get (.-index-map t) key default))

(let [not-found #?(:clj (Object.) :cljs #js {})]
  (defn index-of [t key]
    (let [idx (maybe-index-of t key not-found)]
      (if (identical? idx not-found)
        (throw (ex-info "Key not in struct" {:key key
                                             :available (.-keys t)}))
        idx))))

(defn keys "Vector of keys in order given in [[create]]." [^ClosedStruct t]
  (.-keys t))

(defn keyset [^ClosedStruct t]
  (.-keyset t))

(defn extended-struct "The struct that t extends, or nil." [^ClosedStruct t]
  (.-extended-struct t))
