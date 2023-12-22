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

