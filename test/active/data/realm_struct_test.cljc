(ns active.data.realm-struct-test
  (:require [active.data.realm :as realm]
            [active.data.realm.validation :as realm-validation]
            [active.data.struct :as struct #?@(:cljs [:include-macros true])]
            [active.data.realm-struct :refer [def-realm-struct]]
            #?(:cljs [cljs.test :as t])
            #?(:clj [clojure.test :refer :all]))
  #?(:cljs (:require-macros [cljs.test :refer (is deftest run-tests testing)])))

(def-realm-struct T
  [f1 realm/int
   f2 realm/string])

(def-realm-struct ExtT
  :extends T
  [f3 realm/keyword])

(deftest realm
  (is (realm/record? (realm/compile T))))

(deftest simple-validation
  (is (struct/is-a? T
                    (realm-validation/checking
                     (struct/struct-map T f1 5 f2 "foo"))))
  (is (thrown? Exception
               (realm-validation/checking
                (struct/struct-map T f1 "bar" f2 "foo")))))

(deftest extended-realm-structs
  (is (realm/record? (realm/compile ExtT)))

  (is (struct/is-a? ExtT
                    (realm-validation/checking
                     (struct/struct-map ExtT f1 5 f2 "foo" f3 :bar))))

  (is (thrown? Exception
               (realm-validation/checking
                (struct/struct-map ExtT f1 f1 5 f2 "foo" f3 "test")))))
