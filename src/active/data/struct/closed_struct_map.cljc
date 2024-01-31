(ns ^:no-doc active.data.struct.closed-struct-map
  (:require [active.data.struct.closed-struct :as closed-struct]
            [active.data.struct.closed-struct-data :as data]
            [active.data.struct.key :as struct-key]
            [active.data.struct.validator :as v])
  #?(:clj (:import (clojure.lang Util)))
  (:refer-clojure :rename {instance? clj-instance?
                           satisfies? clj-satisfies?}
                  :exclude [#?@(:cljs [satisfies? instance?])
                            accessor struct-map struct create-struct]))


(defn- maybe-validate! [m t changed-keys changed-values]
  ;; validate map, if the struct has any of the changed-keys
  (when-let [v (closed-struct/get-validator t)]
    ;; OPT: maybe validators should be able to say that they are 'inactive', so that we don't have to do this:
    (when-let [red (->> (map (fn [k v]
                               (when (closed-struct/contains? t k)
                                 [k v]))
                             changed-keys changed-values)
                        (remove nil?)
                        (not-empty))]
      (let [changed-keys (map first red)
            changed-values (map second red)]
        (when-let [s (closed-struct/extended-struct t)]
          (maybe-validate! m s changed-keys changed-values))
        
        (v/validate! v m changed-keys changed-values)))))

(defn- validate [m t changed-keys changed-values]
  ;; validator of extended struct, only if its fields changed:
  (when-let [s (closed-struct/extended-struct t)]
    (maybe-validate! m s changed-keys changed-values))
  ;; but this one always:
  (when-let [v (closed-struct/get-validator t)]
    (v/validate! v m changed-keys changed-values))
  m)

(defn- validate-single! [t changed-key changed-value]
  (when-let [s (closed-struct/extended-struct t)]
    (when (closed-struct/contains? s changed-key)
      (validate-single! s changed-key changed-value)))
  (when-let [v (closed-struct/get-validator t)]
    (v/validate-single! v changed-key changed-value)))

(defn- validate-map-only [m t]
  (when-let [s (closed-struct/extended-struct t)]
    (validate-map-only m s))
  (when-let [v (closed-struct/get-validator t)]
    (v/validate-map-only! v m))
  m)

(defn valid? [t m]
  (and (if-let [s (closed-struct/extended-struct t)]
         (valid? s m)
         true)
       (if-let [validator (closed-struct/get-validator t)]
         (v/valid? validator m)
         true)))

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

;; TODO: other exceptions are probably more helpful: (also synchronize with exceptions thrown in struct/index-of)
(defn- cannot-add [key]
  #?(:clj (Util/runtimeException "Key already present")
     :cljs (js/Error. "Key already present")))

(defn- cannot-remove [key]
  #?(:clj (Util/runtimeException "Can't remove struct key")
     :cljs (js/Error. "Can't remove struct key")))

(declare create)


(defn- ensure-editable! [owner]
  (when-not owner
    ;; same error and message as clojure
    (throw #?(:clj (java.lang.IllegalAccessError. "Transient used after persistent! call")
              :cljs (js/Error. "Transient used after persistent! call")))))

(defn- unchecked-assoc! [struct data key val]
  ;; unchecked = without ensure-editable!
  (validate-single! struct key val)
  (if-let [index (struct-key/optimized-for? key struct)]
    (data/unsafe-mutate! data index val)
    (data/mutate! struct data key)))

(defprotocol ^:private TransientUtil ;; to share some code between clj/cljs, with hopefully little runtime overhead.
  (t-assoc [this key val])
  (t-dissoc [this key])
  (t-persistent [this])
  (t-get [this key])
  (t-get-with-default [this key not-found]))

(deftype ^:private TransientClosedStructMap [struct data #?(:clj ^:unsynchronized-mutable owner :cljs ^:mutable owner)]
  ;; Note: does 'single field' validation immediately, but full map validation only on persistence.

  TransientUtil
  (t-assoc [this key val]
    (ensure-editable! owner)
    (unchecked-assoc! struct data key val)
    this)

  (t-dissoc [this key]
    (ensure-editable! owner)
    (throw (cannot-remove key)))
  
  (t-persistent [this]
    (ensure-editable! owner)
    ;; do full map validation here.
    (set! (.-owner this) false)
    ;; transient -> persistent looses meta.
    (-> (create struct data nil)
        (validate-map-only struct)))

  (t-get [this key]
    (ensure-editable! owner)
    (if-let [index (struct-key/optimized-for? key struct)]
      (data/unsafe-access data index)
      (data/access struct data key)))
  (t-get-with-default [this key not-found]
    (ensure-editable! owner)
    (if-let [index (struct-key/optimized-for? key struct)]
      (data/unsafe-access data index)
      (data/access-with-default struct data key not-found)))
  
  #?@(:clj
      ;; clojure.lang.ATransientMap does not expose any helpers :-/
      ;; TODO: IFn?
      [clojure.lang.ITransientMap
       clojure.lang.Counted
       clojure.lang.ITransientAssociative
       
       (assoc [this key val] (t-assoc this key val))
       (without [this key] (t-dissoc this key))
       
       (count [this] (closed-struct/size struct))
       (conj [this val]
             (ensure-editable! owner)
             (condp clj-instance? val
               java.util.Map$Entry
               (let [^java.util.Map$Entry e val]
                 (unchecked-assoc! struct data (.getKey e) (.getValue e)))

               clojure.lang.IPersistentVector
               (let [^clojure.lang.IPersistentVector v val]
                 (when (not= 2 (.count v))
                   (throw (java.lang.IllegalArgumentException. "Vector arg to map conj must be a pair")))
                 (unchecked-assoc! struct data (.nth v 0) (.nth v 1)))

               ;; else, assume sequence of Map.Entry
               (doseq [o (seq val)]
                 (let [^java.util.Map$Entry e o]
                   (unchecked-assoc! struct data (.getKey e) (.getValue e)))))
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
               (ensure-editable! owner)
               (cond
                 (map-entry? o)
                 (unchecked-assoc! struct data (key o) (val o))
                 
                 (vector? o)
                 (do (when (not= 2 (count o))
                       (throw (js/Error. "Vector arg to map conj must be a pair")))
                     (unchecked-assoc! struct data (o 0) (o 1)))

                 :else
                 (loop [es (seq o)]
                   (when-let [e (first es)]
                     (unchecked-assoc! struct data (key e) (val e))
                     (recur (next es)))))
               this)
       (-persistent! [this] (t-persistent this))

       ICounted
       (-count [this] (closed-struct/size struct))

       ILookup
       (-lookup [this key] (t-get this key))
       (-lookup [this key not-found] (t-get-with-default this key not-found))
       ]))

(defprotocol ^:private PersistentUtil ;; to share some code between clj/cljs, with hopefully little runtime overhead.
  (do-get [this key])
  (do-get-with-default [this key not-found])
  (do-assoc [this key val])
  (do-assoc-multi [this changed-keys-vals])
  (do-transient [this])
  (do-empty [this])
  (do-struct-equiv [this other]))

(deftype ^:private PersistentClosedStructMap [struct data _meta
                                              #?(:clj ^:unsynchronized-mutable ^int _hasheq) ;; only clj!
                                              #?(:clj ^:unsynchronized-mutable ^int _hash :cljs ^:mutable _hash)]

  PersistentUtil
  (do-get [this key]
    (if-let [index (struct-key/optimized-for? key struct)]
      (data/unsafe-access data index)
      (data/access struct data key)))

  (do-get-with-default [this key not-found]
    (if-let [index (struct-key/optimized-for? key struct)]
      (data/unsafe-access data index)
      (data/access-with-default struct data key not-found)))

  (do-assoc [this key val]
    ;; OPT: check if current association is identical?
    (-> (create struct (if-let [index (struct-key/optimized-for? key struct)]
                         (data/unsafe-mutate! (data/copy data) index val)
                         (data/mutate! struct (data/copy data) key val))
                _meta)
        (validate struct (list key) (list val))))

  (do-empty [this]
    ;; OPT: 'memoize' the empty val in struct? we can't create it before hand because of the validation :-/
    (-> (create struct (data/create struct) _meta)
        ;; potentially all keys have changed, to nil
        (validate struct (closed-struct/keys struct) (repeat (closed-struct/size struct) nil))))

  (do-transient [this]
    (TransientClosedStructMap. struct (data/copy data) true))
  
  (do-assoc-multi [this changed-keys-vals]
    (let [changed-keys (map first changed-keys-vals)
          changed-vals (map second changed-keys-vals)]
               
      (-> (create struct (reduce (fn [data [k v]]
                                   (if-let [index (struct-key/optimized-for? k struct)]
                                     (data/unsafe-mutate! data index v)
                                     (data/mutate! struct data k v)))
                                 (data/copy data)
                                 changed-keys-vals)
                  _meta)
          (validate struct changed-keys changed-vals))))

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
       (size [this] (closed-struct/size struct))
       (isEmpty [this] (zero? (closed-struct/size struct)))
       (containsKey [this k] (closed-struct/contains? struct k))
       (keySet [this] ;; Java api, and a Java Set
               (closed-struct/keyset struct))
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
       (withMeta [this meta] (create struct data meta))
       (meta [this] _meta)

       clojure.lang.IHashEq
       (hasheq [this] ;; called by (hash x)
               (when (= 0 (.-_hasheq this))
                 (set! (.-_hasheq this) (clojure.lang.APersistentMap/mapHasheq this)))
               (.-_hasheq this))
       (hashCode [this] ;; Java's hashCode
                 (when (= 0 ^int (.-_hash this))
                   (set! (.-_hash this) (clojure.lang.APersistentMap/mapHash this)))
                 (.-_hash this))

       ;; MapEquivalence marks that other maps should try to compare with this.
       clojure.lang.MapEquivalence

       clojure.lang.IPersistentMap
       (equiv [this other]
              (condp clj-instance? other
                PersistentClosedStructMap
                (do-struct-equiv this ^PersistentClosedStructMap other)
                

                clojure.lang.IPersistentMap
                ;; let other map implementations do the work.
                (.equiv ^clojure.lang.IPersistentMap other this)

                ;; else java maps, or anything else.
                (clojure.lang.APersistentMap/mapEquals this other)))
       (equals [this other] ;; Java's equals
               (clojure.lang.APersistentMap/mapEquals this other))

       (cons [this o]
             ;; Note: 'into' uses this if IEditableCollection is not implemented (and that otherwise)
             (do-assoc-multi this (java-cons-o o)))

       (seq [this] ;; seq of MapEntry
            (iterator-seq (.iterator this)))

       (iterator [this] ;; iterator over MapEntry
                 (data/java-iterator struct data))

       (assoc [this key val] (do-assoc this key val))
       (assocEx [this key val]
                ;; assocEx is 'assoc if not set yet' - all our keys are always set.
                (throw (cannot-add key)))
       (without [this key] (throw (cannot-remove key)))
       (count [this] (closed-struct/size struct))
  
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
       (-clone [this] (PersistentClosedStructMap. struct data meta _hash))

       IWithMeta
       (-with-meta [this meta] (create struct data meta))
       IMeta
       (-meta [this] _meta)

       ICollection
       (-conj [this entry]
              (do-assoc-multi this (js-conj-o entry)))

       IEmptyableCollection
       (-empty [this] (do-empty this))

       IEquiv
       (-equiv [this other]
               (cond
                 (clj-instance? PersistentClosedStructMap other)
                 (do-struct-equiv this ^PersistentClosedStructMap other)

                 (clj-satisfies? IEquiv other)
                 (-equiv other this) ;; fingers crossed they don't do the same.

                 :else false))

       IHash
       (-hash [this] (caching-hash this hash-unordered-coll _hash))

       IIterable
       (-iterator [this] (data/js-iterator struct data))

       ISeqable
       (-seq [coll] (data/js-seq struct data))

       ICounted
       (-count [this] (closed-struct/size struct))

       ILookup
       (-lookup [this key] (do-get this key))
       (-lookup [this key not-found] (do-get-with-default this key not-found))

       IAssociative
       (-assoc [this k v] (do-assoc this k v))
       (-contains-key? [this k] (closed-struct/contains? struct k))

       IFind
       (-find [this key]
              (let [v (do-get-with-default this key ::not-found)]
                (when-not (= v ::not-found)
                  (MapEntry. key v nil))))

       IMap
       (-dissoc [this key] (throw (cannot-remove key)))

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

(defn- create [struct data meta]
  #?(:clj (PersistentClosedStructMap. struct data meta 0 0)
     :cljs (PersistentClosedStructMap. struct data meta nil)))


#?(#_:clj
   ;; seems already implemented for all IPersistentMap
   #_(defmethod print-method PersistentClosedStructMap [^PersistentClosedStructMap s ^java.io.Writer writer]
       (print-method (into {} s) writer))

   :cljs
   (extend-protocol IPrintWithWriter
     PersistentClosedStructMap
     (-pr-writer [^PersistentClosedStructMap s writer x]
       ;; OPT: direct access to map printer? reimplement?
       (-pr-writer (into {} s) writer x))))

(defn- derived-struct? [t vt]
  ;; if vt extends t
  (when-let [p (closed-struct/extended-struct vt)]
    (or (= t p)
        (derived-struct? t p))))

(defn exact-instance? [t v]
  (assert (closed-struct/closed-struct? t)) ;; TODO: exception?
  (and (clj-instance? PersistentClosedStructMap v)
       (= t (.-struct v))))

(defn derived-instance? [t v]
  ;; Note: false if 'exact-instance?'
  (assert (closed-struct/closed-struct? t)) ;; TODO: exception?
  (and (clj-instance? PersistentClosedStructMap v)
       (derived-struct? t (.-struct v))))

#_(defn instance-or-derived? [t v]
  (assert (closed-struct/closed-struct? t)) ;; TODO: exception?
  (and (clj-instance? PersistentClosedStructMap v)
       (or (= t (.-struct v))
           (derived-struct? t (.-struct v)))))

(defn satisfies? [t v]
  (assert (closed-struct/closed-struct? t)) ;; TODO: exception?
  (or (and (clj-instance? PersistentClosedStructMap v)
           (or (= t (.-struct v))
               (derived-struct? t (.-struct v))))
      ;; OPT: don't do this is v is a (different) struct-map
      (and (map? v)
           (and (every? #(contains? v %) (closed-struct/keys t))
                ;; Note: passes v to validator, which may contain more keys. (validators should allow that)
                (valid? t v)))))

(defn unvalidated-empty-transient [struct]
  ;; even if an all-nil map would be invalid, if it is immediately
  ;; used as a transient, the validity is checked when getting
  ;; persistent again.
  (TransientClosedStructMap. struct (data/create struct) true))

(defn- build-map* [struct key-val-pairs]
  ;; TODO: fail if not all keys given, or not? (records allowed that to some extend - via less fields in ctor)
  ;; TODO: if allowing the same key multiple times, then each occurrence is validated; even if overriden later; is that what we want?
  (assert (closed-struct/closed-struct? struct)) ;; TODO: exception
  (let [data (data/create struct)]
    (doseq [[k v] key-val-pairs]
      ;; Note: unchecked-assoc! does validate-field!
      (unchecked-assoc! struct data k v))
    (-> (create struct
                data
                nil)
        (validate-map-only struct))))

(defn build-map-positional [struct vals]
  (assert (closed-struct/closed-struct? struct))
  (let [data (data/create struct vals)] ;; Note: data/create checks arity
    (doseq [[key val] (map vector (closed-struct/keys struct) vals)]
      (validate-single! struct key val))
    (-> (create struct
                data
                nil)
        (validate-map-only struct))))

(defn from-coll [struct coll]
  (assert (= (set (map first coll)) (closed-struct/keyset struct))) ;; TODO: 'key not in struct' exception? or assert and ignore?
  (build-map* struct coll))

(defn from-map [struct m]
  #_(build-map* struct (seq m))
  
  (assert (= (set (keys m)) (closed-struct/keyset struct))) ;; TODO: 'key not in struct' exception? or assert and ignore?
  (build-map-positional struct
                        (map m
                             (closed-struct/keys struct))))

(defn build-map [struct keys-vals]
  ;; alternative; slightly slower currently:
  #_(build-map* struct
                (partition 2 keys-vals))

  ;; Note: this already removes duplicates.
  (from-map struct (apply hash-map keys-vals)))

(defn struct-of-map [m]
  (assert (clj-instance? PersistentClosedStructMap m))
  (.-struct m))

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
  (let [keys (closed-struct/keys struct)
        nkeys (count keys)
        has-validation? (some? (closed-struct/get-validator struct))]
    (fn [& args]
      (assert (= (count args) nkeys))
      (when has-validation?
        (doseq [[k v] (map vector keys args)]
          (validate-single! struct k v)))
      (cond-> (create struct
                      (let [data (data/create struct)]
                        (doseq [[idx v] (map-indexed vector args)]
                          (data/unsafe-mutate! data idx v))
                        data)
                      nil)
        has-validation? (validate-map-only struct)))))

(defmacro gen-positional [n]
  (let [keys (repeatedly n #(gensym "k"))
        vars (repeatedly n #(gensym "v"))
        struct (gensym "struct")
        data (gensym "data")]
    `(fn [~struct]
       (let [[~@keys] (closed-struct/keys ~struct)
             has-validation?# (some? (closed-struct/get-validator ~struct))]
         ;; OPT: we could check if struct is an extended struct, and optimize a few things based on that.
         (fn [~@vars]
           (when has-validation?#
             (do ~@(map (fn [k v]
                          `(validate-single! ~struct ~k ~v))
                        keys
                        vars)))
           (cond-> (create ~struct
                           (let [~data (data/create ~struct)]
                             (do
                               ~@(map-indexed (fn [idx v]
                                                `(data/unsafe-mutate! ~data ~idx ~v))
                                              vars))
                             ~data)
                           nil)
             has-validation?# (validate-map-only ~struct)))))))

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
  (case (count (closed-struct/keys struct))
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
