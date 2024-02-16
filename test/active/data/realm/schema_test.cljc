(ns active.data.realm.schema-test
  (:require [active.data.realm :as realm]
            [active.data.realm.schema :refer [schema]]
            [schema.core :as schema]
            #?(:cljs [cljs.test :refer (deftest is testing)])
            #?(:clj [clojure.test :refer (deftest is testing)]))
    #?(:cljs (:require-macros [cljs.test :refer (is deftest run-tests testing)])))

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

(defprotocol Indexed
  (index [x index]))

(defrecord Pare [kar kdr]
  Indexed
  (index [_ index]
    (case index
      (0) kar
      (1) kdr)))

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

    
