(ns active.data.realm-test
  (:require [active.data.realm :as realm]
            [active.data.realm-schema :refer [schema]]
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

  



