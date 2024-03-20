(ns ^:no-doc active.data.struct.internal.closed-struct-data
  "Mutable data structure backing struct maps."
  (:require [active.data.struct.internal.struct-type :as struct-type]))

(defn unsafe-create ^objects [size]
  (object-array size))

(defn create
  (^objects [struct]
   (unsafe-create (struct-type/size struct)))
  (^objects [struct values]
   (when-not (= (struct-type/size struct) (count values))
     (throw (ex-info (str "Wrong number of values to create closed-struct-data. Expected " (struct-type/size struct) ", but got " (count values))
                     {:struct struct})))
   (to-array values)))

(defn unsafe-access [^objects data ^long index]
  (aget data index))

(defn kv-reduce [struct ^objects data f init]
  (let [keys (struct-type/keys struct)
        len (struct-type/size struct)]
    (loop [res init
           idx 0]
      (if (< idx len)
        (let [res (f res (nth keys idx) (aget data idx))]
          (if (reduced? res)
            @res
            (recur res (inc idx))))
        res))))

(defn unsafe-mutate! [^objects data ^long index value]
  (aset data index value))

(defn copy ^objects [^objects data]
  (aclone data))

(defn equiv [^objects v1 ^objects v2]
  ;; OPT: nothing built in clojure for this?
  (reduce (fn [r idx]
            (if-not r
              (reduced r)
              (= (aget v1 idx)
                 (aget v2 idx))))
          (= (alength v1) (alength v2))
          (range (alength v1))))

(defn indices ^java.lang.Iterable [struct]
  (range (struct-type/size struct)))

#?(:clj
   (defn java-iterator [struct data]
     (let [ks (struct-type/keys struct)
           idxs (indices struct)]
       (let [^java.util.Iterator base (.iterator idxs)]
         (reify java.util.Iterator
           (hasNext [this]
             (.hasNext base))
           (next [this]
             (let [idx (.next base)]
               (clojure.lang.MapEntry. (nth ks idx) (unsafe-access data idx))))
           (remove [this]
             (throw (java.lang.UnsupportedOperationException.)))))))

   :cljs
   (defn js-iterator [struct data]
     (let [ks (struct-type/keys struct)
           idxs (indices struct)]
       (let [base (-iterator idxs)]
         (reify Object
           (hasNext [_] (.hasNext ^js base))
           (next [_]
             (let [idx (.next ^js base)]
               (MapEntry. (nth ks idx) (unsafe-access data idx) nil)))))))
   )

#?(:cljs
   (defn js-seq [struct data]
     ;; OPT: special implementation needed? (but that's a lot: https://github.com/clojure/clojurescript/blob/219951678b16575ec29bb9b88047356cf1437aec/src/main/cljs/cljs/core.cljs#L6808)
     (map (fn [key idx]
            (MapEntry. key (unsafe-access data idx) nil))
          (struct-type/keys struct)
          (indices struct))))
