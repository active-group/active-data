(ns ^:no-doc active.data.struct.closed-struct-map
  (:require [active.data.struct.struct-type :as struct-type]
            [active.data.struct.closed-struct-data :as data]
            [active.data.struct.key :as struct-key]
            [active.data.struct.validator :as v]
            [clojure.set :as set])
  #?(:clj (:import (clojure.lang Util)))
  #?(:cljs (:require-macros [active.data.struct.closed-struct-map :refer [gen-positional]]))
  (:refer-clojure :rename {instance? clj-instance?
                           satisfies? clj-satisfies?}
                  :exclude [#?@(:cljs [satisfies? instance?])
                            get-validator
                            accessor struct-map struct create-struct]))

#_(defprotocol IStructMapVariant ;; -> type?
  (-print-prefix [this] "Optional prefix before the printout of the map - ie. \"{key value ...}\" ")
  )

#_(defn- maybe-validate! [m t changed-keys changed-values]
  ;; validate map, if the struct has any of the changed-keys
  (when-let [v (struct-type/get-current-validator! t)]
    ;; OPT: maybe validators should be able to say that they are 'inactive', so that we don't have to do this:
    (when-let [red (->> (map (fn [k v]
                               (when (struct-type/contains? t k)
                                 [k v]))
                             changed-keys changed-values)
                        (remove nil?)
                        (not-empty))]
      (let [changed-keys (map first red)
            changed-values (map second red)]
        (v/validate! v m changed-keys changed-values)))))

(defn- validate [m t changed-keys changed-values]
  (when-let [v (struct-type/get-current-validator! t)]
    (v/validate! v m changed-keys changed-values))
  m)

(defn- validate-single! [t changed-key changed-value]
  (when-let [v (struct-type/get-current-validator! t)]
    (v/validate-single! v changed-key changed-value)))

(defn- validate-multi! [t changed-keys-vals]
  (when-let [v (struct-type/get-current-validator! t)]
    (doseq [[changed-key changed-value] changed-keys-vals]
      (v/validate-single! v changed-key changed-value))))

(defn- validate-map-only [m t]
  (when-let [v (struct-type/get-current-validator! t)]
    (v/validate-map-only! v m))
  m)

#_(defn valid? [t m]
  (if-let [validator (closed-struct/get-current-validator! t)]
    (v/valid? validator m)
    true))

#?(:clj
   (defn- java-cons-o [o]
     (condp clj-instance? o
       java.util.Map$Entry
       (let [^java.util.Map$Entry e o]
         (list (list (.getKey e) (.getValue e))))

       clojure.lang.IPersistentVector
       (let [^clojure.lang.IPersistentVector v o]
         ;; should be tuple [k v]
         (when (not= (count v) 2)
           ;; same as APersistentMap does:
           (throw (IllegalArgumentException. "Vector arg to map conj must be a pair")))
         (list (list (.nth v 0) (.nth v 1))))

       ;; else: clojure.lang.IPersistentMap
       (let [^clojure.lang.IPersistentMap m o]
         (map (fn [^java.util.Map$Entry e]
                (list (.getKey e) (.getValue e)))
              (.seq m)))))
   :cljs
   (defn- js-conj-o [o]
     (cond
       (vector? o) (list (list (-nth o 0) (-nth o 1)))
       :else
       (map (fn [o]
              (if (vector? o)
                (list (-nth o 0) (-nth o 1))
                ;; same error as cljs:
                (throw (js/Error. "conj on a map takes map entries or seqables of map entries"))))
            o))))

(defn- cannot-add [key]
  (ex-info (str "Key already present: " key) {:key key}))

(defn- unknown-key [key struct-type]
  (ex-info (str "Unknown key: " key) {:key key :struct struct-type}))

(defn- cannot-remove [key]
  (ex-info (str "Cannot remove keys") {}))

(declare create)


(defn- ensure-editable! [owner]
  (when-not owner
    ;; same error and message as clojure
    (throw #?(:clj (java.lang.IllegalAccessError. "Transient used after persistent! call")
              :cljs (js/Error. "Transient used after persistent! call")))))

(defn- export [struct-map]
  (into {} struct-map))

(defn- export-transient! [tmap]
  (transient (export (persistent! tmap))))

(defprotocol ^:private TransientUtil ;; to share some code between clj/cljs, with hopefully little runtime overhead.
  (t-find-index-of [this key])
  (t-assoc [this key val])
  (t-assoc-multi [this keys-vals])
  (t-dissoc [this key])
  (t-persistent [this])
  (t-get [this key])
  (t-get-with-default [this key not-found]))

(deftype ^:private TransientClosedStructMap [struct data locked? #?(:clj ^:unsynchronized-mutable owner :cljs ^:mutable owner)]
  ;; Note: does 'single field' validation immediately, but full map validation only on persistence.

  TransientUtil
  (t-find-index-of [this key]
    (or (struct-key/optimized-for? key struct)
        (struct-type/maybe-index-of struct key nil)))
  
  (t-assoc [this key val]
    (ensure-editable! owner)
    (if-let [index (t-find-index-of this key)]
      (do (validate-single! struct key val)
          (data/unsafe-mutate! data index val)
          this)
      (if locked?
        (throw (unknown-key key struct))
        (assoc! (export-transient! this) key val))))

  (t-assoc-multi [this keys-vals]
    (ensure-editable! owner)
    (let [keys (map first keys-vals)
          indices (map #(t-find-index-of this %1) keys)]
      (if (not (every? some? indices))
        (if locked?
          (throw (unknown-key (first (remove #(t-find-index-of this %1) keys)) struct))
          (reduce (fn [res [k v]]
                    (assoc! res k v))
                (export-transient! this)))
        (do (validate-multi! struct keys-vals)
            (doseq [[index val] (map vector indices (map second keys-vals))]
              (data/unsafe-mutate! data index val))
            this))))
  
  (t-dissoc [this key]
    (ensure-editable! owner)
    (if locked?
      (throw (cannot-remove key))
      (dissoc! (export-transient! this) key)))
  
  (t-persistent [this]
    (ensure-editable! owner)
    ;; do final map validation here.
    (set! (.-owner this) false)
    ;; transient -> persistent looses meta.
    (-> (create struct data locked? nil)
        (validate-map-only struct)))

  (t-get [this key]
    (ensure-editable! owner)
    (if-let [index (t-find-index-of this key)]
      (data/unsafe-access data index)
      (throw (unknown-key key struct))))
  (t-get-with-default [this key not-found]
    (ensure-editable! owner)
    (if-let [index (t-find-index-of this key)]
      (data/unsafe-access data index)
      not-found))
  
  #?@(:clj
      ;; clojure.lang.ATransientMap does not expose any helpers :-/
      ;; TODO: IFn?
      [clojure.lang.ITransientMap
       clojure.lang.Counted
       clojure.lang.ITransientAssociative
       
       (assoc [this key val] (t-assoc this key val))
       (without [this key] (t-dissoc this key))
       
       (count [this] (struct-type/size struct))
       (conj [this val]
             (condp clj-instance? val
               java.util.Map$Entry
               (let [^java.util.Map$Entry e val]
                 (t-assoc this (.getKey e) (.getValue e)))

               clojure.lang.IPersistentVector
               (let [^clojure.lang.IPersistentVector v val]
                 (when (not= 2 (.count v))
                   (throw (java.lang.IllegalArgumentException. "Vector arg to map conj must be a pair")))
                 (t-assoc this (.nth v 0) (.nth v 1)))

               ;; else, assume sequence of Map.Entry
               (t-assoc-multi this (map (fn [^java.util.Map$Entry e]
                                          [(.getKey e) (.getValue e)])
                                        (seq val))))
             this)
       (valAt [this key] (t-get this key))
       (valAt [this key not-found] (t-get-with-default this key not-found))
       
       (persistent [this] (t-persistent this))
       ]
      :cljs
      ;; TODO: IFn?
      [ITransientAssociative
       (-assoc! [this key val] (t-assoc this key val))

       ITransientMap
       (-dissoc! [this key] (t-dissoc this key))

       ITransientCollection
       (-conj! [this o]
               (cond
                 (map-entry? o)
                 (t-assoc this (key o) (val o))
                 
                 (vector? o)
                 (do (when (not= 2 (count o))
                       (throw (js/Error. "Vector arg to map conj must be a pair")))
                     (t-assoc this (o 0) (o 1)))

                 :else
                 (t-assoc-multi this (map (fn [e]
                                            [(key e) (val e)])
                                          (seq o))))
               this)
       (-persistent! [this] (t-persistent this))

       ICounted
       (-count [this] (struct-type/size struct))

       ILookup
       (-lookup [this key] (t-get this key))
       (-lookup [this key not-found] (t-get-with-default this key not-found))
       ]))

(defprotocol ^:private PersistentUtil ;; to share some code between clj/cljs, with hopefully little runtime overhead.
  (find-index-of [this key])
  (do-get [this key])
  (do-get-with-default [this key not-found])
  (do-assoc [this key val])
  (do-dissoc [this key])
  (do-assoc-multi [this changed-keys-vals])
  (do-transient [this])
  (do-empty [this])
  (do-struct-equiv [this other])
  (do-optional-struct-type= [this other])
  (do-optional-struct-type-hash [this]))

(deftype ^:private PersistentClosedStructMap [struct data locked? _meta
                                              #?(:clj ^:unsynchronized-mutable ^int _hasheq) ;; only clj!
                                              #?(:clj ^:unsynchronized-mutable ^int _hash :cljs ^:mutable _hash)]

  PersistentUtil
  (find-index-of [this key]
    (or (struct-key/optimized-for? key struct)
        (struct-type/maybe-index-of struct key nil)))

  (do-get [this key]
    (if-let [index (find-index-of this key)]
      (data/unsafe-access data index)
      (if locked?
        (throw (unknown-key key struct))
        nil)))

  (do-get-with-default [this key not-found]
    (if-let [index (find-index-of this key)]
      (data/unsafe-access data index)
      not-found))

  (do-assoc [this key val]
    ;; OPT: check if current associated value is identical?
    (if-let [index (find-index-of this key)]
      (-> (create struct (let [d (data/copy data)]
                           (data/unsafe-mutate! d index val)
                           d)
                  locked? _meta)
          (validate struct (list key) (list val)))
      (if locked?
        (throw (unknown-key key struct))
        (assoc (export this) key val))))

  (do-dissoc [this key]
    (if locked?
      (throw (cannot-remove key))
      (dissoc (export this) key)))

  (do-empty [this]
    ;; a struct-map with all nil values, is as empty as this can get.
    
    ;; OPT: 'memoize' the empty val in struct? we can't create it before hand because of the validation :-/
    (-> (create struct (data/create struct) locked? _meta)
        ;; potentially all keys have changed, to nil
        (validate struct (struct-type/keys struct) (repeat (struct-type/size struct) nil))))

  (do-transient [this]
    (TransientClosedStructMap. struct (data/copy data) locked? true))
  
  (do-assoc-multi [this changed-keys-vals]
    (let [changed-keys (map first changed-keys-vals)
          changed-vals (map second changed-keys-vals)]

      (let [[new-data irritant]
            (reduce (fn [[data _] [k v]]
                      (if-let [index (find-index-of this k)]
                        (do (data/unsafe-mutate! data index v)
                            [data nil])
                        (reduced [nil k])))
                    [(data/copy data) nil]
                    changed-keys-vals)]
        (if (nil? new-data)
          (if locked?
            (throw (unknown-key irritant struct))
            (into (export this) changed-keys-vals))
          (-> (create struct new-data locked? _meta)
              (validate struct changed-keys changed-vals))))))

  (do-optional-struct-type= [this other]
    ;; if an 'identifier' is defined for the struct-type, then other
    ;; must be a struct-map with the same identifier.
    (let [id (struct-type/identifier (.-struct this))]
      (or (nil? id)
          (and (clj-instance? PersistentClosedStructMap other)
               (= id (struct-type/identifier (.-struct ^PersistentClosedStructMap other)))))))

  (do-optional-struct-type-hash [this]
    (let [id (struct-type/identifier (.-struct this))]
      (if (some? id)
        (hash id)
        0)))
  
  (do-struct-equiv [this other]
    (and (= struct (.-struct ^PersistentClosedStructMap other))
         (data/equiv data (.-data ^PersistentClosedStructMap other))))

  ;; TODO: Ifn taking key.
  #?@(:clj
      ;; TODO: IKVReduce ? (looks like an optimization)
      
      ;; Note: if we used gen-class instead of deftype, we could
      ;; extend APersistentMap; but using gen-class is also not easy
      ;; in terms of the interop.
      [java.lang.Iterable
       
       java.util.Map
       (size [this] (struct-type/size struct))
       (isEmpty [this] (zero? (struct-type/size struct)))
       (containsKey [this k] (struct-type/contains? struct k))
       (keySet [this] ;; Java api, and a Java Set
               (struct-type/keyset struct))
       (getOrDefault [this key default] (do-get-with-default this key default))
       (get [this key]
            ;; Note: this is called by clojure to compare other maps
            ;; to this; but maybe in more situations.
            
            ;; Not sure if we should/could throw here too, if a key is
            ;; not in map, or stick to the java.Map contract by
            ;; returning null.
            (do-get-with-default this key nil))

       clojure.lang.IObj
       clojure.lang.IMeta
       (withMeta [this meta] (create struct data locked? meta))
       (meta [this] _meta)

       clojure.lang.IHashEq
       (hasheq [this] ;; called by (hash x)
               (when (= 0 (.-_hasheq this))
                 (set! (.-_hasheq this) (+ (do-optional-struct-type-hash this)
                                           (clojure.lang.APersistentMap/mapHasheq this))))
               (.-_hasheq this))
       (hashCode [this] ;; Java's hashCode
                 (when (= 0 ^int (.-_hash this))
                   (set! (.-_hash this) (+ (do-optional-struct-type-hash this)
                                           (clojure.lang.APersistentMap/mapHash this))))
                 (.-_hash this))

       ;; MapEquivalence marks that other maps should try to compare with this.
       clojure.lang.MapEquivalence

       clojure.lang.IPersistentMap
       (equiv [this other]
              (and (do-optional-struct-type= this other)
                   (condp clj-instance? other
                     PersistentClosedStructMap
                     (do-struct-equiv this ^PersistentClosedStructMap other)
                

                     clojure.lang.IPersistentMap
                     ;; let other map implementations do the work.
                     (.equiv ^clojure.lang.IPersistentMap other this)

                     ;; else java maps, or anything else.
                     (clojure.lang.APersistentMap/mapEquals this other))))
       (equals [this other] ;; Java's equals
               (and (do-optional-struct-type= this other)
                    (clojure.lang.APersistentMap/mapEquals this other)))

       (cons [this o]
             ;; Note: 'into' uses this if IEditableCollection is not implemented (and that otherwise)
             (do-assoc-multi this (java-cons-o o)))

       (seq [this] ;; seq of MapEntry
            (iterator-seq (.iterator this)))

       (iterator [this] ;; iterator over MapEntry
                 (data/java-iterator struct data))

       (assoc [this key val] (do-assoc this key val))
       (assocEx [this key val]
                ;; the semantic should be 'assoc unless already present, throw otherwise'
                ;; all actual keys are always set though.
                (if (or locked? (find-index-of this key))
                  (throw (cannot-add key))
                  (assoc (export this) key val)))
       (without [this key]
                (do-dissoc this key))
       (count [this] (struct-type/size struct))
  
       (valAt [this key] (do-get this key))
       (valAt [this key not-found] (do-get-with-default this key not-found))
       (empty [this] (do-empty this))

       clojure.lang.IEditableCollection
       (asTransient [this] (do-transient this))
       ]
      :cljs
      [Object
       (toString [this]
                 ;; this is (str v), see pr-writer below.
                 (pr-str* this))
       (equiv [this other] (-equiv this other))
       ICloneable
       (-clone [this] (PersistentClosedStructMap. struct data locked? meta _hash))

       IWithMeta
       (-with-meta [this meta] (create struct data locked? meta))
       IMeta
       (-meta [this] _meta)

       ICollection
       (-conj [this entry]
              (do-assoc-multi this (js-conj-o entry)))

       IEmptyableCollection
       (-empty [this] (do-empty this))

       IEquiv
       (-equiv [this other]
               (and (do-optional-struct-type= this other)
                    (cond
                      (clj-instance? PersistentClosedStructMap other)
                      (do-struct-equiv this ^PersistentClosedStructMap other)

                      (clj-satisfies? IEquiv other)
                      (-equiv other this) ;; fingers crossed they don't do the same.

                      :else false)))

       IHash
       (-hash [this] (caching-hash this (fn [v]
                                          (+ (do-optional-struct-type-hash v)
                                             (hash-unordered-coll v)))
                                   _hash))

       IIterable
       (-iterator [this] (data/js-iterator struct data))

       ISeqable
       (-seq [coll] (data/js-seq struct data))

       ICounted
       (-count [this] (struct-type/size struct))

       ILookup
       (-lookup [this key] (do-get this key))
       (-lookup [this key not-found] (do-get-with-default this key not-found))

       IAssociative
       (-assoc [this k v] (do-assoc this k v))
       (-contains-key? [this k] (struct-type/contains? struct k))

       IFind
       (-find [this key]
              (let [v (do-get-with-default this key ::not-found)]
                (when-not (= v ::not-found)
                  (MapEntry. key v nil))))

       IMap
       (-dissoc [this key] (do-dissoc this key))

       IKVReduce
       (-kv-reduce [this f init] (data/kv-reduce struct data f init))

       IReduce
       (-reduce [coll f]
                (iter-reduce coll f))
       (-reduce [coll f start]
                (iter-reduce coll f start))

       ;; IFn
       ;; (-invoke [coll k]
       ;;          (-lookup coll k))

       ;; (-invoke [coll k not-found]
       ;;          (-lookup coll k not-found))

       IEditableCollection
       (-as-transient [this] (do-transient this))
       ]
      ))

;; TODO: for PersistentArrayMap, there are a bunch of static methods added in cljs.core (via set!); are those needed here too?
#?(:cljs (es6-iterable PersistentClosedStructMap))

(defn- create [struct data locked? meta]
  #?(:clj (PersistentClosedStructMap. struct data locked? meta 0 0)
     :cljs (PersistentClosedStructMap. struct data locked? meta nil)))

#?(:clj
   ;; seems already implemented for all IPersistentMap, but we override it
   (defmethod print-method PersistentClosedStructMap [^PersistentClosedStructMap s ^java.io.Writer writer]
     (.write writer (struct-type/print-map-prefix (.-struct s)))
     (print-method (into {} s) writer))

   :cljs
   (extend-protocol IPrintWithWriter
     PersistentClosedStructMap
     (-pr-writer [^PersistentClosedStructMap s writer x]
       (-pr-writer (struct-type/print-map-prefix (.-struct s)) writer x)
       ;; OPT: direct access to map printer? reimplement?
       (-pr-writer (into {} s) writer x))))

(defn struct-map? [v]
  (clj-instance? PersistentClosedStructMap v))

(defn exact-instance? [t v]
  (assert (struct-type/struct-type? t))
  (and (clj-instance? PersistentClosedStructMap v)
       (= t (.-struct ^PersistentClosedStructMap v))))

(defn satisfies? [t v]
  (assert (struct-type/struct-type? t))
  (or (and (clj-instance? PersistentClosedStructMap v)
           (= t (.-struct ^PersistentClosedStructMap v)))
      ;; OPT: don't do this is v is a (different) struct-map
      (and (map? v)
           (and (every? #(contains? v %) (struct-type/keys t))
                ;; Note: passes v to validator, which may contain more keys. (validators should allow that)
                ;; TODO: add back or remove?
                #_(valid? t v)))))

(defn unvalidated-empty-transient [struct]
  ;; even if an all-nil map would be invalid, if it is immediately
  ;; used as a transient, the validity is checked when getting
  ;; persistent again.
  (TransientClosedStructMap. struct (data/create struct) (struct-type/locked-maps? struct) true))

(defn- build-map* [struct key-val-pairs]
  (assert (struct-type/struct-type? struct))

  ;; for locked maps, keys are checked in t-assoc-multi, but even for unlocked maps,
  ;; we don't want to immediately export the map on creation.
  (when-not (struct-type/locked-maps? struct)
    ;; TODO: exception or assert?
    (assert (empty? (set/difference (set (map first key-val-pairs)) (struct-type/keyset struct)))))
  
  (-> (t-assoc-multi (unvalidated-empty-transient struct) key-val-pairs)
      (t-persistent)))

(defn build-map-positional [struct vals]
  (assert (struct-type/struct-type? struct))
  (build-map* struct (map vector (struct-type/keys struct) vals)))

(defn from-coll [struct coll]
  (build-map* struct coll))

(defn build-map [struct keys-vals]
  (build-map* struct
              (partition 2 keys-vals)))

(defn struct-of-map [^PersistentClosedStructMap m]
  (assert (clj-instance? PersistentClosedStructMap m))
  (.-struct m))

(defn lock-struct-map [m]
  (create (struct-of-map m) (.-data m) true (meta m)))

(defn unlock-struct-map [m]
  (create (struct-of-map m) (.-data m) false (meta m)))

(defn accessor [struct key]
  ;; TODO: actually optimize it, e.g by looking at key beforehand.
  (fn [m]
    (get m key)))

(defn mutator [struct key]
  ;; TODO: actually optimize it, e.g by looking at key beforehand.
  (fn [m v]
    (assoc m key v)))

(defn mutator! [struct key]
  ;; TODO: actually optimize it, e.g by looking at key beforehand.
  (fn [m v]
    (assoc! m key v)))

(defn positional-n [struct]
  (let [keys (struct-type/keys struct)
        nkeys (count keys)]
    (fn [& args]
      (assert (= (count args) nkeys))
      (validate-multi! struct (map vector keys args))
      (-> (create struct
                  (let [data (data/create struct)]
                    (doseq [[idx v] (map-indexed vector args)]
                      (data/unsafe-mutate! data idx v))
                    data)
                  (struct-type/locked-maps? struct)
                  nil)
          (validate-map-only struct)))))

#?(:clj
   (defmacro gen-positional [n]
     (let [keys (repeatedly n #(gensym "k"))
           vars (repeatedly n #(gensym "v"))
           struct (gensym "struct")
           data (gensym "data")]
       ;; TODO: optimize/fix validator calls
       `(fn [~struct]
          (let [[~@keys] (struct-type/keys ~struct)
                has-validation?# (some? (struct-type/get-validator ~struct))
                locked?# (struct-type/locked-maps? ~struct)
                size# (struct-type/size ~struct)]
            (fn [~@vars]
              (when has-validation?#
                (do ~@(map (fn [k v]
                             `(validate-single! ~struct ~k ~v))
                           keys
                           vars)))
              (cond-> (create ~struct
                              (let [~data (data/unsafe-create size#)]
                                (do
                                  ~@(map-indexed (fn [idx v]
                                                   `(data/unsafe-mutate! ~data ~idx ~v))
                                                 vars))
                                ~data)
                              locked?#
                              nil)
                has-validation?# (validate-map-only ~struct))))))))

(def positional-1 (gen-positional 1))
(def positional-2 (gen-positional 2))
(def positional-3 (gen-positional 3))
(def positional-4 (gen-positional 4))
(def positional-5 (gen-positional 5))
(def positional-6 (gen-positional 6))
(def positional-7 (gen-positional 7))
(def positional-8 (gen-positional 8))
(def positional-9 (gen-positional 9))
(def positional-10 (gen-positional 10))
(def positional-11 (gen-positional 11))
(def positional-12 (gen-positional 12))
(def positional-13 (gen-positional 13))
(def positional-14 (gen-positional 14))
(def positional-15 (gen-positional 15))
(def positional-16 (gen-positional 16))
(def positional-17 (gen-positional 17))
(def positional-18 (gen-positional 18))
(def positional-19 (gen-positional 19))
(def positional-20 (gen-positional 20))

(defn positional-constructor [struct]
  (case (count (struct-type/keys struct))
    1 (positional-1 struct)
    2 (positional-2 struct)
    3 (positional-3 struct)
    4 (positional-4 struct)
    5 (positional-5 struct)
    6 (positional-6 struct)
    7 (positional-7 struct)
    8 (positional-8 struct)
    9 (positional-9 struct)
    10 (positional-10 struct)
    11 (positional-11 struct)
    12 (positional-12 struct)
    13 (positional-13 struct)
    14 (positional-14 struct)
    15 (positional-15 struct)
    16 (positional-16 struct)
    17 (positional-17 struct)
    18 (positional-18 struct)
    19 (positional-19 struct)
    20 (positional-20 struct)
    ;; else
    (positional-n struct)))
