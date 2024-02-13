(ns active.data.struct-test
  (:require [active.data.struct :as sut #?@(:cljs [:include-macros true])]
            #_[active.data.struct.validator :as validator]
            #_[active.clojure.lens :as lens]
            [clojure.data :as data]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(def ^:foo T (sut/struct [:t-a :t-b]))

(t/deftest metadata-test
  (= {:bar :baz} (meta (with-meta T {:bar :baz})))
  
  (t/is (:foo (meta #'T))))

(defn throws [t]
  #?(:clj (try (t) nil (catch Throwable e e))
     :cljs (try (t) nil (catch :default e e))))

(t/deftest construction-test
  (let [v (sut/struct-map T
                          :t-a 42
                          :t-b :foo)]
    (t/is (some? v) "Can construct with all fields")

    (t/is (= v (sut/struct-map T
                               :t-b :foo
                               :t-a 42)) "order does not matter")

    (t/is (= v (sut/to-struct-map T [[:t-b :foo]
                                     [:t-a 42]]))))

  (t/testing "IFn on struct"
    (let [v (sut/struct-map T
                            :t-a 42
                            :t-b :foo)]
      (t/is (= (T :t-a 42
                  :t-b :foo) v))
      (t/is (= (T {:t-a 42
                   :t-b :foo}) v))))

  (t/is (some? (sut/struct-map T :t-a 42))
        "Can construct with partial fields")
  
  (t/is (some? (throws #(sut/struct-map T :foo 42)))
        "Cannot construct with foreign fields")

  ;; Note: not sure if we want to allow this; but it could be an easy
  ;; way to write 'smart constructors' that define default values.
  (t/is (= (sut/struct-map T
                           :t-b :foo
                           :t-a 42)
           (sut/struct-map T
                           :t-b :foo
                           :t-a nil
                           :t-a 42))
        "Later values 'override' previous values in struct-map")

  (t/testing "precalculated positional ctor"
    (let [ctor (sut/constructor T)]
      (t/is (= (sut/struct-map T
                               :t-a 42
                               :t-b :foo)
               (ctor 42 :foo))))))

(t/deftest empty-struct-test
  ;; empty structs don't make much sense, as they are all equal but ok..
  (let [EmptyT1 (sut/struct [])
        EmptyT2 (sut/struct [])]
    (t/is (= (sut/struct-map EmptyT1)
             (EmptyT2)
             {}))))

(t/deftest printer-test
  ;; (also indirectly tests key printer)
  (let [s (pr-str (sut/struct-map T
                                  :t-a "42"
                                  :t-b :foo))]
    (t/is (#{"{:t-b :foo, :t-a \"42\"}"
             "{:t-a \"42\", :t-b :foo}"} s)))
  (let [s (str (sut/struct-map T
                               :t-a "42"
                               :t-b :foo))]
    (t/is #{"{:t-b :foo, :t-a 42}"
            "{:t-a 42, :t-b :foo}"} s))
  )

(t/deftest access-test
  (let [v (sut/struct-map T
                          :t-a 42
                          :t-b :foo)]
    (t/testing "via keys"
      (t/is (= 42 (:t-a v))))
    
    (t/testing "via get"
      (t/is (= :foo (get v :t-b)))
      (t/is (= :foo (get-in {:test v} [:test :t-b]))))
    
    (t/testing "via destructuring"
      (let [{a :t-a b :t-b} v]
        (t/is (= 42 a))
        (t/is (= :foo b))))

    (t/testing "access of other keys"
      (t/is (nil? (get v :bar))))))

(t/deftest empty-fn-test
  (let [v (sut/struct-map T
                          :t-a 42
                          :t-b :foo)]
    (t/is (= #{:t-a :t-b}
             (set (keys (empty v)))))
    (t/is (= (sut/struct-map T :t-a nil :t-b nil)
             (empty v)))))

(t/deftest modify-test
  (let [v (sut/struct-map T
                          :t-a 42
                          :t-b :foo)]
    (t/testing "via assoc"
      (t/is (= (sut/struct-map T
                               :t-a 42
                               :t-b :bar)
               (assoc v :t-b :bar))))

    (t/testing "via update"
      (t/is (= (sut/struct-map T
                               :t-a -42
                               :t-b :foo)
               (update v :t-a -))))

    (t/testing "changes map type for keys no in struct"
      (t/is (= {:t-a 42
                :t-b :foo
                :bar "z"}
               (assoc v :bar "z"))))
    ))


(t/deftest equality-test
  (let [v (sut/struct-map T
                          :t-a 42
                          :t-b :foo)
        v2 (sut/struct-map T
                           :t-a 42
                           :t-b :foo)
        v3 {:t-a 42
            :t-b :foo}]

    (t/testing "equality"
      (t/is (= :t-a :t-a) "Key equal itself")

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
                          :t-a 42
                          :t-b :foo)]

    (t/testing "struct-maps are maps"
      (t/is (map? v)))

    (t/testing "has-keys?"
      (t/is (sut/has-keys? T (sut/struct-map T
                                             :t-a -42
                                             :t-b :foo)))
      
      (t/is (sut/has-keys? T {:t-a nil
                              :t-b :foo
                              :x :y}))

      (t/is (not (sut/has-keys? T {})))
      
      (t/is (not (sut/has-keys? T {:t-a 42}))))
    ))

(t/deftest reflection-test
  ;; requires iterator to be implemented
  (let [v (sut/struct-map T
                          :t-a 42
                          :t-b :foo)]
    (t/testing "reduce and reduce-kv"
      (t/is (= #{[:t-a 42] [:t-b :foo]}
               (reduce (fn [r [k v]]
                         (conj r [k v]))
                       #{}
                       v)))
      (t/is (= #{[:t-a 42] [:t-b :foo]}
               (reduce-kv (fn [r k v]
                            (conj r [k v]))
                          #{}
                          v))))

    (t/testing "keys"
      (t/is #{:t-a :t-b}
            (set (keys v))))
    (t/testing "vals"
      (t/is #{42 :foo}
            (set (vals v))))))

(t/deftest into-test
  (let [v (sut/struct-map T
                          :t-a 42
                          :t-b :foo)]
    (t/is (= #{[:t-a 42] [:t-b :foo]}
             (into #{} v))))

  (let [v (sut/struct-map T
                          :t-a 42
                          :t-b :foo)]
    ;; requires cons to be implemented, unless transient is implemented.
    (t/is (= v
             (into (sut/struct-map T :t-a nil :t-b nil)
                   #{[:t-a 42] [:t-b :foo]})))))


(t/deftest number-of-fields-test
  ;; more than 21 fields works.
  (let [BigT (sut/struct [::t-a0 ::t-a1 ::t-a2 ::t-a3 ::t-a4 ::t-a5 ::t-a6 ::t-a7 ::t-a8 ::t-a9
                          ::t-a10 ::t-a11 ::t-a12 ::t-a13 ::t-a14 ::t-a15 ::t-a16 ::t-a17 ::t-a18 ::t-a19
                          ::t-a20 ::t-a21 ::t-a22 ::t-a23 ::t-a24 ::t-a25 ::t-a26 ::t-a27 ::t-a28 ::t-a29])]
    (t/is (some? (sut/struct-map BigT
                                 ::t-a0 nil ::t-a1 nil ::t-a2 nil ::t-a3 nil ::t-a4 nil ::t-a5 nil ::t-a6 nil ::t-a7 nil ::t-a8 nil ::t-a9 nil
                                 ::t-a10 nil ::t-a11 nil ::t-a12 nil ::t-a13 nil ::t-a14 nil ::t-a15 nil ::t-a16 nil ::t-a17 nil ::t-a18 nil ::t-a19 nil
                                 ::t-a20 nil ::t-a21 nil ::t-a22 nil ::t-a23 nil ::t-a24 nil ::t-a25 nil ::t-a26 nil ::t-a27 nil ::t-a28 nil ::t-a29 nil)))))

(t/deftest transient-test
  (let [v (sut/struct-map T
                          :t-a 42
                          :t-b :foo)]
    (t/is (= (sut/struct-map T
                             :t-a 21
                             :t-b :foo)
             (persistent! (-> (transient v)
                              (assoc! :t-a 21)))))
    (t/is (throws #(let [x (transient v)]
                     (persistent! x)
                     (assoc! x :t-a 21))))
    ))

;; TODO
#_(t/deftest map-projection-test
  (let [p (sut/map-projection T {:t-a (lens/>> :foo :bar)
                                 :t-b :b})
        v (sut/struct-map T :t-a 42 :t-b :test)
        m {:foo {:bar 42}
           :b :test}]

    (t/is (= (lens/yank v p) m))
    (t/is (= (lens/shove nil p m) v)))

  (t/is (throws #(sut/map-projection T {:t-b :b}))
        "All fields must be given.")

  (t/testing "optimized yank if only keywords"
    (let [p (sut/map-projection T {:t-a :a
                                   :t-b :b})
          v (sut/struct-map T :t-a 42 :t-b :test)
          m {:a 42
             :b :test}]

      (t/is (= (lens/yank v p) m))
      (t/is (= (lens/shove nil p m) v)))
    ))

(t/deftest diff-and-humane-test-output-test
  ;; watch live:
  #_(t/is (= (sut/struct-map T :t-a 42 :t-b :foo)
             (sut/struct-map T :t-a 42 :t-b :bar)))

  ;; humane-test-output also relies on clojure.data/diff
  (t/is (= [{:t-b :foo} {:t-b :bar} {:t-a 42}]
           (data/diff (sut/struct-map T :t-a 42 :t-b :foo)
                      (sut/struct-map T :t-a 42 :t-b :bar))))
  )

(t/deftest destructuring-works-test
  (let [{a :t-a b :t-b} (sut/struct-map T :t-a 42 :t-b :foo)]
    (t/is (= a 42))
    (t/is (= b :foo))))
