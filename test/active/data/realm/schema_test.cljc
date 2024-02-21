(ns active.data.realm.schema-test
  (:require [active.data.realm :as realm]
            [active.data.realm.schema :refer [schema]]
            [schema.core :as schema]
            #?(:cljs [cljs.test :refer (deftest is testing)])
            #?(:clj [clojure.test :refer (deftest is testing)]))
  #?(:cljs (:require-macros [cljs.test :refer [is deftest run-tests testing]])))

(deftest builtin-scalar-realm-test
  (is (some? (schema/validate (schema realm/int)
                              12)))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (schema/validate (schema realm/int)
                                "12"))))

(deftest predicate-realm-test
  (is (some?
       (schema/validate (schema (realm/predicate "odd integer" odd?))
                        11)))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (schema/validate (schema (realm/predicate "odd integer" odd?))
                                12))))


(deftest optional-realm-test
  (is (some?
       (schema/validate (schema (realm/optional realm/int))
                        12)))

  (is (nil?
       (schema/validate (schema (realm/optional realm/int))
                        nil)))

  (is (thrown? #?(:clj Exception :cljs js/Error)
               (schema/validate (schema (realm/optional (realm/int)))
                                "12"))))


(deftest tuple-realm-test
  (let [s (schema (realm/compile [realm/string realm/keyword realm/int]))]
    (is (some?
         (schema/validate s ["foo" :bar 42])))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (schema/validate s [:foo :bar 42])))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (schema/validate s ["foo" "bar" 42])))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (schema/validate s ["foo" :bar "42"])))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (schema/validate s ["foo" :bar])))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (schema/validate s ["foo" :bar 42 43])))))


(deftest map-of-realm-test
  (let [s (schema (realm/compile {realm/keyword realm/string}))]
    (is
     (some?
      (schema/validate s {:foo "bar"
                          :baz "blam"})))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (schema/validate s {:foo :bar
                                     :baz "blam"})))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (schema/validate s {"foo" "bar"
                                     :baz "blam"})))))

(deftest integer-from-to-test
  (let [s (schema (realm/integer-from-to 1 10))]
    (is (some? (schema/validate s 1)))
    (is (some? (schema/validate s 5)))
    (is (some? (schema/validate s 10)))

    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 0)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 11)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s "11")))))

(deftest union-test
  (let [s (schema (realm/union realm/string realm/keyword realm/int))]
    (is (some? (schema/validate s "foo")))
    (is (some? (schema/validate s :foo)))
    (is (some? (schema/validate s 5)))

    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 'foo)))))

(deftest sequence-of-test
  (let [s (schema (realm/sequence-of realm/string))]
    (is (some? (schema/validate s [])))
    (is (some? (schema/validate s '())))
    (is (some? (schema/validate s ["foo"])))
    (is (some? (schema/validate s '("foo"))))
    (is (some? (schema/validate s ["foo" "bar"])))

    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 'foo)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s [:foo :bar])))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s ["foo" :bar])))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s [:foo "bar"])))))

(deftest set-of-test
  (let [s (schema (realm/set-of realm/string))]
    (is (some? (schema/validate s #{})))
    (is (some? (schema/validate s #{"foo"})))
    (is (some? (schema/validate s #{"foo" "bar"})))

    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s [])))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s '())))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 'foo)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s #{:foo :bar})))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s #{"foo" :bar})))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s #{:foo "bar"})))))

(deftest map-with-keys-test
  (let [s (schema (realm/map-with-keys {:foo realm/string :bar (realm/optional realm/int)}))]
    (is (some? (schema/validate s {:foo "foo" :bar 15})))
    (is (some? (schema/validate s {:foo "foo"})))

    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s {})))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s {:bar 15})))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s {:bar "15"})))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s {:foo 15})))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s [])))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s '())))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 'foo)))))

(deftest enum-test
  (let [s (schema (realm/enum :a "a" 'b 1 2 [4]))]
    (is (some? (schema/validate s :a)))
    (is (some? (schema/validate s 'b)))
    (is (some? (schema/validate s 1)))
    (is (some? (schema/validate s 2)))
    (is (some? (schema/validate s [4])))
    (is (some? (schema/validate s '(4))))

    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s :b)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 'c)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 3)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 4)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s [5])))))


(deftest intersection-test
  (let [s (schema (realm/intersection realm/int
                                      (realm/predicate "non-negative number" (fn [x] (>= x 0)))))]
    
    (is (some? (schema/validate s 5)))
    (is (some? (schema/validate s 0)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s :a)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 5.5)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s -1)))))

(deftest restricted-test
  (let [s (schema (realm/restricted realm/int
                                    (fn [x] (>= x 0))
                                    "non-negative ints"))]

    (is (some? (schema/validate s 5)))
    (is (some? (schema/validate s 0)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s :a)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 5.5)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s -1)))))

(defrecord Pare [kar kdr])

(def pare-realm
  (realm/record "Pare"
                ->Pare
                (partial instance? Pare)
                [(realm/field "kar" realm/int :kar)
                 (realm/field "kdr" realm/double :kdr)]))

(deftest record-test
  (let [s (schema pare-realm)]
    (is (some? (schema/validate s (->Pare 1 2))))
    
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s :a)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s [1 2])))))

(deftest named-test
  (let [s (schema (realm/named :a realm/int))]

    (is (some? (schema/validate s 5)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 5.5)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s :a)))))


(def plist-realm
  (realm/union (realm/enum '())
               (realm/record "plist-pare"
                             ->Pare
                             (partial instance? Pare)
                             [(realm/field "kar" realm/int :kar)
                              (realm/field "kdr" (realm/delay plist-realm) :kdr)])))

(deftest delay-test
  (let [s (schema plist-realm)]

    (is (some? (schema/validate s '())))
    (is (some? (schema/validate s (->Pare 5 '()))))
    (is (some? (schema/validate s (->Pare 3 (->Pare 5 '())))))

    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s 5)))
    ;; note this one does not throw an exception - that's supposed to be done by the record constructor
    #_(is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s (->Pare :a '()))))
    ))

(deftest function-test
  (let [s1 (schema (realm/function realm/int realm/int -> realm/boolean))
        s2 (schema (realm/function realm/int realm/int & (realm/string) -> realm/boolean))
        s3 (schema (realm/function realm/int realm/int & [realm/string realm/int realm/boolean] -> realm/boolean))
        s4 (schema (realm/function realm/int realm/int & {:a realm/string :b realm/int :c realm/boolean} -> realm/boolean))
        s5 (schema (realm/function-cases
                    (realm/function realm/int realm/int -> realm/boolean)
                    (realm/function realm/int realm/int & (realm/string) -> realm/boolean)))]

    ;; only rudimentary checking
    (is (some? (schema/validate s1 (fn [i1 i2] true))))
    (is (some? (schema/validate s2 (fn [i1 i2 & ss] true))))
    (is (some? (schema/validate s3 (fn [i1 i2 & [s1 i3 b3]] true))))
    (is (some? (schema/validate s4 (fn [i1 i2 & {:keys [a b c] :or {a :a b :b c :c}}] [a b c]))))
    (is (some? (schema/validate s5 (fn [i1 i2] true))))
    (is (some? (schema/validate s5 (fn [i1 i2 & ss] true))))

    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s1 5)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s2 5)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s3 5)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s4 5)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (schema/validate s5 5)))))

        
    
    

    

        

                                
                                
