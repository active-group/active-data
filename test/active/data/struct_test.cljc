(ns active.data.struct-test
  (:require [active.data.struct :as sut #?@(:cljs [:include-macros true])]
            [active.data.struct.validator :as validator]
            [active.data.struct.key :as key]
            [active.data.struct.closed-struct-meta :as closed-struct-meta]
            #_[active.clojure.lens :as lens]
            [clojure.data :as data]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(sut/def-struct ^:foo T [^:a t-a t-b])

(sut/def-struct ^:private PrivT [pt-a ^{:private false} pt-b])

(t/deftest metadata-test
  (t/is (= (str (var T))
           (str "#'" (get (meta T) closed-struct-meta/name-meta-key))))
  
  (t/is (:foo (meta #'T)))
  (t/is (:a (meta #'t-a)))

  (t/testing "private inheritance"
    (t/is (:private (meta #'PrivT)))
    (t/is (:private (meta #'pt-a)) "Privateness is inherited")
    (t/is (not (:private (meta #'pt-b))) "Privateness is inherited only as a default")))

(t/deftest optimization-test
  (t/is (key/optimized-for? t-a T))
  (t/is (not (key/optimized-for? t-a PrivT)))

  ;; Note: ClosedStructMap must also make use of that, but that cannot be tested extrinsicly.
  )

(defn throws [t]
  #?(:clj (try (t) nil (catch Throwable e e))
     :cljs (try (t) nil (catch :default e e))))

(t/deftest construction-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/is (some? v) "Can construct with all fields")

    (t/is (= v (sut/struct-map T
                               t-b :foo
                               t-a 42)) "order does not matter"))

  ;; TODO? That takes some time so check, esp. when also allowing the same key multiple times.
  #_(t/is (some? (throws #(sut/struct-map T t-a 42)))
        "Cannot construct with partial fields")
  
  (t/is (some? (throws #(sut/struct-map T :foo 42)))
        "Cannot construct with foreign fields")

  ;; Note: not sure if we want to allow this; but it could be an easy
  ;; way to write 'smart constructors' that define default values.
  (t/is (= (sut/struct-map T
                           t-b :foo
                           t-a 42)
           (sut/struct-map T
                           t-b :foo
                           t-a nil
                           t-a 42))
        "Later values 'override' previous values in struct-map")

  (t/testing "precalculated positional ctor"
    (let [ctor (sut/constructor T)]
      (t/is (= (sut/struct-map T
                               t-a 42
                               t-b :foo)
               (ctor 42 :foo))))))

(t/deftest printer-test
  ;; (also indirectly tests key printer)
  (let [s (pr-str (sut/struct-map T
                                  t-a "42"
                                  t-b :foo))]
    (t/is (#{"{~t-b :foo, ~t-a \"42\"}"
             "{~t-a \"42\", ~t-b :foo}"} s)))
  (let [s (str (sut/struct-map T
                               t-a "42"
                               t-b :foo))]
    (t/is #{"{~t-b :foo, ~t-a 42}"
            "{~t-a 42, ~t-b :foo}"} s))
  )

(t/deftest access-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/testing "via keys"
      (t/is (= 42 (t-a v))))
    
    (t/testing "via get"
      (t/is (= :foo (get v t-b)))
      (t/is (= :foo (get-in {:test v} [:test t-b]))))
    
    (t/testing "via destructuring"
      (let [{a t-a b t-b} v]
        (t/is (= 42 a))
        (t/is (= :foo b))))

    (t/testing "access fails if keys not in struct-map"
      (t/is (throws #(:foo v)))
      (t/is (throws #(get v :foo)))
      (t/is (throws #(let [{foo :foo} v]))))
    ))

(t/deftest empty-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/is (= #{t-a t-b}
             (set (keys (empty v)))))
    (t/is (= (sut/struct-map T t-a nil t-b nil)
             (empty v)))))

(t/deftest modify-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/testing "via assoc"
      (t/is (= (sut/struct-map T
                               t-a 42
                               t-b :bar)
               (assoc v t-b :bar))))

    (t/testing "via key fn"
      (t/is (= (sut/struct-map T
                               t-a 42
                               t-b :bar)
               (t-b v :bar))))
    
    (t/testing "via update"
      (t/is (= (sut/struct-map T
                               t-a -42
                               t-b :foo)
               (update v t-a -))))

    (t/testing "update fails if key not in struct-map"
      (t/is (throws #(assoc v :foo 42))))
    ))

(t/deftest keyed-map-test
  (t/testing "keys also work for hash-maps"
    (let [v {t-a 42
             t-b :foo}]
      (t/testing "access"
        (t/is (= 42 (t-a v)))
        (t/is (= :foo (get v t-b)))

        (t/is (= nil (t-b {})))
        (t/is (= nil (get {} t-b)))
        )

      (t/testing "update"
       (t/is (= {t-a 42 t-b :bar}
                (t-b v :bar)))
    
       (t/is (= {t-a 42 t-b :bar}
                (assoc v t-b :bar))))))
  )

(t/deftest compare-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)
        v2 (sut/struct-map T
                           t-a 42
                           t-b :foo)
        v3 {t-a 42
            t-b :foo}]

    (t/testing "equality"
      (t/is (= t-a t-a) "Key equal itself")

      (t/is (= T T) "Types equal itself")

      (t/is (= v v) "Equals itself")

      (t/is (and (= v v2) (= v2 v)) "Equals other struct-map")

      (t/is (= v v3) "Equals other maps with same keys")
      (t/is (= v3 v) "Other maps with same keys are equal")

      (t/is (not (= v {:foo 42})) "Does not equals maps with other keys")
      (t/is (not (= {:foo 42} v)) "Maps with other keys are not equal"))
    
    (t/testing "hash"
      (t/is (= (hash v3) (hash v)) "Hash equals that of an equal map")
      )
    ))

(t/deftest type-test-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]

    (t/testing "struct-maps are maps"
      (t/is (map? v)))
    
    (t/testing "instance?"
      (t/is (sut/instance? T v))

      (t/is (not (sut/instance? T 42)))
      (t/is (not (sut/instance? T {}))))

    (t/testing "satisfies?"
      (t/is (not (sut/satisfies? T 42)))
      
      (t/is (sut/satisfies? T {t-a nil
                               t-b :foo
                               :x :y}))

      (t/is (not (sut/satisfies? T {t-a 42}))))

    #_(t/testing "normal maps can be ok too"
      ;; option: allow this or not?
      (t/is (sut/instance? T {t-a 42
                              t-b :foo}))

      (t/is (not (sut/instance? T {t-a 42})))
      )
    ))

(t/deftest reflection-test
  ;; requires iterator to be implemented
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/testing "reduce and reduce-kv"
      (t/is (= #{[t-a 42] [t-b :foo]}
               (reduce (fn [r [k v]]
                         (conj r [k v]))
                       #{}
                       v)))
      (t/is (= #{[t-a 42] [t-b :foo]}
               (reduce-kv (fn [r k v]
                            (conj r [k v]))
                          #{}
                          v))))

    (t/testing "keys"
      (t/is #{t-a t-b}
            (set (keys v))))
    (t/testing "vals"
      (t/is #{42 :foo}
            (set (vals v))))))

(t/deftest into-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/is (= #{[t-a 42] [t-b :foo]}
             (into #{} v))))

  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    ;; requires cons to be implemented, unless transient is implemented.
    (t/is (= v
             (into (sut/struct-map T t-a nil t-b nil)
                   #{[t-a 42] [t-b :foo]})))

    (t/is (throws
           #(into (sut/struct-map T t-a nil t-b nil)
                  #{[:bar :baz]}))
          "cannot add other keys")))


#?(:clj
   (t/deftest non-generative-clj-test
     (let [[t1 v1] (eval '(vector (active.data.struct/def-struct A [a])
                                  (active.data.struct/struct-map A a :foo)))
           [t2 v2] (eval '(vector (active.data.struct/def-struct A [a])
                                  (active.data.struct/struct-map A a :foo)))]
       (t/is (= t1 t2))
       
       (t/is (= v1 v2))

       (t/is (sut/instance? t1 v2))
       (t/is (sut/instance? t2 v1))
       )))

(defrecord AllInt []
  validator/IMapValidator
  (-validate-field! [this changed-key changed-value]
    (when-not (int? changed-value)
      (throw (ex-info "Not an int" {:v changed-value}))))
  (-validate-map! [this m]
    nil))

(t/deftest validator-test

  (sut/def-struct ValidatedT [vt-a vt-b])
  (sut/set-validator! ValidatedT (AllInt.))
  
  (let [valid (sut/struct-map ValidatedT vt-a 42 vt-b 21)]
    (t/testing "contruction checks for validity"
      (t/is (throws #(sut/struct-map ValidatedT vt-a :foo vt-b 21))))

    (t/testing "modification checks for validity"
      (t/is (throws #(assoc valid vt-a :foo)))
      (t/is (throws #(into valid {vt-a :foo})))
      (t/is (throws #(empty valid))))

    (t/testing "satisfies? checks for validity"  
      (t/is (sut/satisfies? ValidatedT {vt-a 11 vt-b 22}))
      (t/is (not (sut/satisfies? ValidatedT {vt-a :foo vt-b :bar})))))
  )

(t/deftest number-of-fields-test
  ;; more than 21 fields works.
  (sut/def-struct BigT [t-a0 t-a1 t-a2 t-a3 t-a4 t-a5 t-a6 t-a7 t-a8 t-a9
                        t-a10 t-a11 t-a12 t-a13 t-a14 t-a15 t-a16 t-a17 t-a18 t-a19
                        t-a20 t-a21 t-a22 t-a23 t-a24 t-a25 t-a26 t-a27 t-a28 t-a29])
  (t/is (sut/instance? BigT
                       (sut/struct-map BigT
                                       t-a0 nil t-a1 nil t-a2 nil t-a3 nil t-a4 nil t-a5 nil t-a6 nil t-a7 nil t-a8 nil t-a9 nil
                                       t-a10 nil t-a11 nil t-a12 nil t-a13 nil t-a14 nil t-a15 nil t-a16 nil t-a17 nil t-a18 nil t-a19 nil
                                       t-a20 nil t-a21 nil t-a22 nil t-a23 nil t-a24 nil t-a25 nil t-a26 nil t-a27 nil t-a28 nil t-a29 nil))))

(t/deftest transient-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/is (= (sut/struct-map T
                             t-a 21
                             t-b :foo)
             (persistent! (-> (transient v)
                              (assoc! t-a 21)))))
    (t/is (throws #(let [x (transient v)]
                     (persistent! x)
                     (assoc! x t-a 21))))
    ))

;; TODO
#_(t/deftest map-projection-test
  (let [p (sut/map-projection T {t-a (lens/>> :foo :bar)
                                 t-b :b})
        v (sut/struct-map T t-a 42 t-b :test)
        m {:foo {:bar 42}
           :b :test}]

    (t/is (= (lens/yank v p) m))
    (t/is (= (lens/shove nil p m) v)))

  (t/is (throws #(sut/map-projection T {t-b :b}))
        "All fields must be given.")

  (t/testing "optimized yank if only keywords"
    (let [p (sut/map-projection T {t-a :a
                                   t-b :b})
          v (sut/struct-map T t-a 42 t-b :test)
          m {:a 42
             :b :test}]

      (t/is (= (lens/yank v p) m))
      (t/is (= (lens/shove nil p m) v)))
    ))

(t/deftest diff-and-humane-test-output-test
  ;; watch live:
  #_(t/is (= (sut/struct-map T t-a 42 t-b :foo)
             (sut/struct-map T t-a 42 t-b :bar)))

  ;; humane-test-output also relies on clojure.data/diff
  (t/is (= [{t-b :foo} {t-b :bar} {t-a 42}]
           (data/diff (sut/struct-map T t-a 42 t-b :foo)
                      (sut/struct-map T t-a 42 t-b :bar))))
  )

(t/deftest destructuring-works-test
  (let [{a t-a b t-b} (sut/struct-map T t-a 42 t-b :foo)]
    (t/is (= a 42))
    (t/is (= b :foo))))

(sut/def-struct ExT
  :extends T
  [t-c])

(t/deftest extends-test
  (let [v (sut/struct-map ExT t-a 42 t-b :test t-c "xxx")]
    (t/is (sut/satisfies? T v))
    (t/is (not (sut/instance? T v)))

    (t/is (sut/instance? ExT (assoc v t-a 10)))
    (t/is (= 10 (t-a (assoc v t-a 10))))
    (t/is (= "yyy" (t-c (assoc v t-c "yyy"))))

    ;; optimizations
    (t/is (key/optimized-for? t-a ExT))
    )

  (t/testing "inherited validation"
    (let [inspect-v (fn [atom]
                      (reify validator/IMapValidator
                        (-validate-field! [this changed-key changed-value]
                          (swap! atom conj [:field changed-key changed-value]))
                        (-validate-map! [this m]
                          (swap! atom conj [:map m]))))
          base-validations (atom [])
          derived-validations (atom [])]
      
      (sut/def-struct VBase [vt-a])
      (sut/set-validator! VBase (inspect-v base-validations))

      (sut/def-struct VExt :extends VBase [vt-b])
      (sut/set-validator! VExt (inspect-v derived-validations))
  
      (let [valid (sut/struct-map VExt vt-a 42 vt-b 21)]
        (t/testing "contruction checks for validity"
          (t/is (= [[:field vt-a 42]
                    [:map valid]]
                   @base-validations))
          (t/is (= [[:field vt-a 42]
                    [:field vt-b 21]
                    [:map valid]]
                   @derived-validations)))

        (reset! base-validations [])
        (reset! derived-validations [])

        (t/testing "modification checks for validity"
          ;; of a base field:
          (assoc valid vt-a :foo)
          (t/is (= `[[:field ~vt-a :foo]
                     [:map {~vt-a :foo, ~vt-b 21}]]
                   @base-validations))
          ;; Note: validator of derived struct is also called for change in base field
          (t/is (= `[[:field ~vt-a :foo]
                     [:map {~vt-a :foo, ~vt-b 21}]]
                   @derived-validations))

          (reset! base-validations [])
          (reset! derived-validations [])

          ;; of a derived field:
          (assoc valid vt-b :bar)
          (t/is (empty? @base-validations))
          (t/is (= `[[:field ~vt-b :bar]
                     [:map {~vt-a 42, ~vt-b :bar}]]
                   @derived-validations)))

        (reset! base-validations [])
        (reset! derived-validations [])

        (t/testing "satisfies? does not check base validity if derived struct-map"
          ;; Note: although it is not an instance? of base, the base validity must have been checked on construction already.
          
          (t/is (sut/satisfies? VBase valid))
          (t/is (empty? @base-validations))
          )))
    )
  
  )
