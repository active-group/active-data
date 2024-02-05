(ns active.data.struct
  (:require [active.data.struct.struct-type :as struct-type]
            [active.data.struct.closed-struct-map :as struct-map]
            #_[active.clojure.lens :as lens])
  (:refer-clojure :exclude [struct-map struct
                            set-validator! get-validator
                            accessor]))

(declare struct-map)
(declare to-struct-map)

(defrecord ^:no-doc StructVariant []
  struct-type/IStructTypeVariant
    (-variant-name [this] "active.data.struct.Struct")
    (-construct-from [this struct-type m]
      (to-struct-map struct-type m))
    (-identifier [this struct-type] nil)
    (-print-map-prefix [this struct-type] "")
    (-locked-maps? [this] false))

(def ^:no-doc struct-variant (StructVariant.))

(defn struct
  "Returns the description of a structure, i.e. maps with the given keys."
  [keys]
  (struct-type/create keys
                      struct-variant
                      nil
                      nil))

(defn struct-map
  "Returns a new struct map with the keys of the struct.

  ```
  (struct-map (struct [field-1 ...]) field-1 42 ...)
  ```
  "
  [struct & keys-vals]
  (struct-map/build-map struct keys-vals))

(defn to-struct-map
  "Returns a new struct map with the keys of the struct, from a collection of key-value tuples.

  ```
  (def T (struct [field-1 ...]))
  (to-struct-map T {field-1 42})
  ```
  "
  [struct keys-vals]
  (struct-map/from-coll struct keys-vals))

(defn struct-keys [struct]
  (struct-type/keys struct))

(defn constructor
  "Returns an optimized positional constructor function for struct-maps
  of the given struct. The order of the arguments to the constructor
  must match that of the definition of the struct, with additional
  fields from extended structs first."
  [struct]
  (struct-map/positional-constructor struct))

(defn accessor
  "Returns an optimized accessor function for the value associated with
  the given key in a struct-map of the given struct."
  [struct key]
  ;; Note: (key m) and even (get m key) is already very
  ;; efficient. This is slightly more efficient; but only use it if
  ;; needed.
  (struct-map/accessor struct key))

(defn mutator
  "Returns an optimized mutator function for the value associated with
  the given key in a struct-map of the given struct."
  [struct key]
  ;; Note: (key m v) and even (assoc m key v) is already very
  ;; efficient. This is slightly more efficient; but only use it if
  ;; needed.
  (struct-map/mutator struct key))

(defn mutator!
  "Returns an optimized mutator function for the value associated with
  the given key in a transient struct-map of the given struct."
  [struct key]
  ;; Note: (assoc! m key v) and even (assoc m key v) is already very
  ;; efficient. This is slightly more efficient; but only use it if
  ;; needed.
  (struct-map/mutator! struct key))

(defn struct-of "Returns the struct the given struct-map was created from." [m]
  (struct-map/struct-of-map m))

(defn ^:no-doc get-validator [struct]
  (struct-type/get-validator struct))

(defn struct?
  "Tests if v is a struct as returned by [[struct]]."
  [v]
  (and (struct-type/struct-type? v)
       (= struct-variant (struct-type/variant v))))

(defn struct-map? [v]
  (struct-map/struct-map? v))

(defn lock [struct-map]
  (assert (struct-map? struct-map))
  (struct-map/lock-struct-map struct-map))

(defn unlock [struct-map]
  (assert (struct-map? struct-map))
  (struct-map/unlock-struct-map struct-map))


(defn has-keys?
  "Tests if `v` is a map that contains at least the keys defined for `struct`."
  [struct v]
  ;; Note: also checks the validity, if a validator is defined for struct.  (TODO: really do validate?)
  (struct-map/satisfies? struct v))

;; TODO: here?
#_(let [from-struct-1 (fn [v struct field-lens-map]
                      (reduce-kv (fn [r f l]
                                   (lens/shove r l (f v)))
                                 {}
                                 field-lens-map))
      from-struct-2 (fn [v struct field-keyword-map]
                      (-> (reduce-kv (fn [r f k]
                                       (assoc! r k (f v)))
                                     (transient {})
                                     field-keyword-map)
                          (persistent!)))
      to-struct (fn [v struct field-lens-map]
                  (-> (reduce-kv (fn [r f l]
                                   (assoc! r f (lens/yank v l)))
                                 (closed-struct-map/unvalidated-empty-transient struct)
                                 field-lens-map)
                      (persistent!)))]
  (defn map-projection "Returns a lens that projects between a struct-map of the given `struct` and a
  hash-map.

  ```
  (def-struct T [foo])

  (def p (map-projection {foo (lens/>> :foo :bar)}))
  
  (= (lens/yank (struct-map T foo 42) p)
     {:foo {:bar 42}})
  
  (= (lens/shove nil p {:foo {:bar 42}})
     (struct-map T foo 42))
  ```
  " [struct field-lens-map] ;; or maybe this should be in the lens package?
    ;; TODO: an optional 'error handler' that informs about which field was transformed when an exception occurred?
    (assert (= (closed-struct/keyset struct) (set (keys field-lens-map))) "All keys of the struct must be given.")
    (lens/xmap (if (every? keyword? (vals field-lens-map)) from-struct-2 from-struct-1)
               to-struct
               struct
               field-lens-map)))
