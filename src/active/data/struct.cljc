(ns active.data.struct
  "Structs describe structured data and offer an optimized way to work with such data.

  ```
  (def person (struct [:name :age]))
  ```

  Values can be tested to see if they conform with the struct:

  ```
  (has-keys? person {:name \"Huge\", :age 37})

  (not (has-keys? person {}))
  ```

  A special kind of optimized struct-maps can be created,
  using [[struct-map]] and [[to-struct-map]] or using the struct as a function:

  ```
  (def p1 (person :name \"Hugo\" :age 27))

  (def p2 (person {:name \"Tina\" :age 31}))
  ```

  Struct-maps support all standard map functions like [[assoc]] and
  [[get]], as well as [[transient]] and [[assoc!]].

  But adding keys not defined in the record, or removing any key will
  change a struct-map into a hash-map! That behaviour can be changed
  using [[lock]] and [[unlock]]. Locked struct maps throw an error in
  these cases.
  
  You can test if something is an actual struct-map:
  
  ```
  (struct-map? p1)

  (not (struct-map? (dissoc p1 :name)))
  ```

  Structs support some reflection at runtime:

  ```
  (= person (struct-of p1))

  (= [:name :age] (struct-keys person))

  (struct? person)
  ```
  "
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

(defn- struct-variant? [v]
  (instance? StructVariant v))

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

(defn struct?
  "Tests if v is a struct as returned by [[struct]]."
  [v]
  (and (struct-type/struct-type? v)
       (struct-variant? (struct-type/variant v))))

(defn struct-map? [v]
  (and (struct-map/struct-map? v)
       (struct-variant? (struct-type/variant (struct-map/struct-of-map v)))))

(defn struct-of "Returns the struct the given struct-map was created from." [m]
  (assert (struct-map? m))
  (struct-map/struct-of-map m))

(defn lock [struct-map]
  (assert (struct-map? struct-map))
  (struct-map/lock-struct-map struct-map))

(defn unlock [struct-map]
  (assert (struct-map? struct-map))
  (struct-map/unlock-struct-map struct-map))

(defn has-keys?
  "Tests if `v` is a map that contains at least the keys defined for `struct`."
  [struct v]
  (assert (map? v))
  (or (and (struct-map? v)
           (= struct (struct-map/struct-of-map v)))
      (every? (set (keys v)) (struct-type/keys struct))))

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
