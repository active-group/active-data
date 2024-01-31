(ns active.data.struct
  (:require [active.data.struct.key :as key]
            [active.data.struct.closed-struct :as closed-struct]
            [active.data.struct.closed-struct-map :as closed-struct-map]
            [active.data.struct.closed-struct-meta :as closed-struct-meta]
            #_[active.clojure.lens :as lens])
  (:refer-clojure :exclude [struct-map instance? satisfies?
                            set-validator! get-validator
                            accessor]))

;; Note: there is no positional constructor on purpose; although they
;; can be handy for small structs, they quickly become hard to read
;; and hard to refactor when getting larger. And defining a positional
;; constructor for small structs is much easier than the other way
;; round.

(defmacro ^:no-doc def-key [name]
  `(def ~name (key/make (symbol ~(str *ns*) ~(str name)))))

(defn ^:no-doc parse-def-struct-args [args]
  ;; ...could use spec for that.
  ;; TODO: not super tight on possible errors...
  (loop [args args
         extends nil
         fields nil]
    (if (empty? args)
      [extends nil fields]
      (cond
        (true? extends) (recur (rest args)
                               (first args)
                               fields)
        :else
        (case (first args)
          :extends (recur (rest args)
                          true
                          fields)
          (recur (rest args)
                 extends
                 (first args)))))))

(defmacro ^:no-doc def-struct*
  [t extends _meta fields]
  (when (empty? fields)
    ;; Note: all empty structs would be equal (and all struct-map instances of all)
    ;; And unique values should probably be created differently anyway.
    (throw (ex-info "Cannot define an empty struct." {:name t})))

  `(do
     ~@(for [f# fields]
         `(def-key ~(cond-> f#
                      (and (contains? (meta t) :private)
                           (not (contains? (meta f#) :private)))
                      (vary-meta assoc :private (:private (meta t))))))

     ;; Note that 'meta' is evaluated after the fields, so they may refer to them.
     (def ~t (closed-struct/create ~fields
                                   ~extends
                                   (assoc ~_meta closed-struct-meta/name-meta-key (symbol (str *ns*) (str '~t)))))

     ~@(for [f# fields]
         `(key/optimize-for! ~f# ~t))
     
     ~t))

(defmacro def-struct
  "Defines a struct and its keys:

  ```
  (def-struct T [field-1 ...])
  ```

  Other structs can be extended, too:

  ``
  (def-struct X
    :extends T
    [field-2])
  ```
  "
  ([t & args]
   (let [[extends _meta fields] (parse-def-struct-args args)]
     `(def-struct* ~t ~extends ~_meta ~fields))))

(defn struct-map
  "Returns a new struct map with the keys of the struct. All keys of the
  stuct must be given.

  ```
  (def-struct T [field-1 ...])
  (struct-map T field-1 42 ...)
  ```
  "
  [struct & keys-vals]
  ;; TODO: reject the same key given twice? Or offer that explicitly as an easy way to specify default values?
  ;; TODO: actually require all keys? If duplicates are allowed, that is costly to check.
  ;; TODO: hash-map are quite complex macros in cljs - check that out.
  (closed-struct-map/build-map struct keys-vals))

(defn to-struct-map
  "Returns a new struct map with the keys of the struct, from a collection of key-value tuples. All keys of the
  stuct must be given.

  ```
  (def-struct T [field-1 ...])
  (to-struct-map T {field-1 42})
  ```
  "
  [struct keys-vals]
  (closed-struct-map/from-coll struct keys-vals))

(defn constructor
  "Returns an optimized positional constructor function for struct-maps
  of the given struct. The order of the arguments to the constructor
  must match that of the definition of the struct, with additional
  fields from extended structs first."
  [struct]
  (closed-struct-map/positional-constructor struct))

(defn accessor
  "Returns an optimized accessor function for the value associated with
  the given key in a struct-map of the given struct."
  [struct key]
  ;; Note: (key m) and even (get m key) is already very
  ;; efficient. This is slightly more efficient; but only use it if
  ;; needed.
  (closed-struct-map/accessor struct key))

(defn mutator
  "Returns an optimized mutator function for the value associated with
  the given key in a struct-map of the given struct."
  [struct key]
  ;; Note: (key m v) and even (assoc m key v) is already very
  ;; efficient. This is slightly more efficient; but only use it if
  ;; needed.
  (closed-struct-map/mutator struct key))

(defn mutator!
  "Returns an optimized mutator function for the value associated with
  the given key in a transient struct-map of the given struct."
  [struct key]
  ;; Note: (assoc! m key v) and even (assoc m key v) is already very
  ;; efficient. This is slightly more efficient; but only use it if
  ;; needed.
  (closed-struct-map/mutator! struct key))

(defn struct-of "Returns the struct the given struct-map was created from." [m]
  (closed-struct-map/struct-of-map m))

(defn ^{:no-doc true
        :doc "Replace validator of struct. Unsynchronized side effect; use only if you know what you are doing."}
  set-validator! [struct validator]
  ;; Note: the validator is not an argument to 'def-struct', because
  ;; you usually want to use the defined keys in the validator
  ;; implementation; that would make for a weird macro.
  (closed-struct/set-validator! struct validator))

(defn ^:no-doc get-validator [struct]
  (closed-struct/get-validator struct))

(defn struct?
  "Tests if v is a struct defined by [[def-struct]]."
  [v]
  (closed-struct/closed-struct? v))

(defn instance?
  "Tests if `v` is a struct map created from the given `struct`."
  [struct v]
  ;; TODO: should a struct-map of a derived struct be an instance of the extended struct? (it currently isn't; only satisfies? allows that)
  (closed-struct-map/instance? struct v))

(defn satisfies?
  "Tests if `v` is a map and contains at least the keys defined for `struct`."
  [struct v]
  ;; Note: also checks the validity, if a validator is defined for struct.
  (closed-struct-map/satisfies? struct v))

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
