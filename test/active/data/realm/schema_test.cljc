(ns active.data.realm.schema-test
  (:require [active.data.realm :as realm]
            [active.data.realm.schema :refer [schema]]
            [schema.core :as schema]
            #?(:cljs [cljs.test :as t])
            #?(:clj [clojure.test :refer :all]))
    #?(:cljs (:require-macros [cljs.test :refer (is deftest run-tests testing)])))

(deftest builtin-scalar-realm-test
  (is (some? (schema/validate (schema realm/int)
                              12)))
  (is (thrown? Exception
               (schema/validate (schema realm/int)
                                "12"))))

(deftest predicate-realm-test
  (is (some?
       (schema/validate (schema (realm/predicate "odd integer" odd?))
                        11)))
  (is (thrown? Exception
               (schema/validate (schema (realm/predicate "odd integer" odd?))
                                12))))


(deftest optional-realm-test
  (is (some?
       (schema/validate (schema (realm/optional realm/int))
                        12)))

  (is (nil?
       (schema/validate (schema (realm/optional realm/int))
                        nil)))

  (is (thrown? Exception
               (schema/validate (schema (realm/optional (realm/int)))
                                "12"))))


(deftest tuple-realm-test
  (let [s (schema (realm/compile [realm/string realm/keyword realm/int]))]
    (is (some?
         (schema/validate s ["foo" :bar 42])))
    (is (thrown? Exception
                 (schema/validate s [:foo :bar 42])))
    (is (thrown? Exception
                 (schema/validate s ["foo" "bar" 42])))
    (is (thrown? Exception
                 (schema/validate s ["foo" :bar "42"])))
    (is (thrown? Exception
                 (schema/validate s ["foo" :bar])))
    (is (thrown? Exception
                 (schema/validate s ["foo" :bar 42 43])))))


(deftest map-of-realm-test
  (let [s (schema (realm/compile {realm/keyword realm/string}))]
    (is
     (some?
      (schema/validate s {:foo "bar"
                          :baz "blam"})))
    (is (thrown? Exception
                 (schema/validate s {:foo :bar
                                     :baz "blam"})))
    (is (thrown? Exception
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

(deftest protocol-test
  (let [s (schema (realm/compile Indexed))]
    (is
     (some?
      (schema/validate s (->Pare :kar :kdr))))
  
    (is (thrown? Exception
                 (schema/validate s [:kar :kdr])))))

(deftest integer-from-to-test
  (let [s (schema (realm/integer-from-to 1 10))]
    (is (some? (schema/validate s 1)))
    (is (some? (schema/validate s 5)))
    (is (some? (schema/validate s 10)))

    (is (thrown? Exception (schema/validate s 0)))
    (is (thrown? Exception (schema/validate s 11)))
    (is (thrown? Exception (schema/validate s "11")))))

(deftest union-test
  (let [s (schema (realm/union realm/string realm/keyword realm/int))]
    (is (some? (schema/validate s "foo")))
    (is (some? (schema/validate s :foo)))
    (is (some? (schema/validate s 5)))

    (is (thrown? Exception (schema/validate s 'foo)))))
