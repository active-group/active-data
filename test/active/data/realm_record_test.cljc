(ns active.data.realm-record-test
  (:require [active.data.realm :as realm]
            [active.data.realm.validation :as realm-validation]
            [active.data.record :as record #?@(:cljs [:include-macros true])]
            [active.data.realm-record :as sut #?@(:cljs [:include-macros true])]
            #?(:cljs [cljs.test :refer-macros (is deftest testing)])
            #?(:clj [clojure.test :refer (is deftest testing)])))

(sut/def-realm-record T
  [f1 realm/int
   f2 realm/string])

(sut/def-realm-record ExtT
  :extends T
  [f3 realm/keyword])

(deftest realm
  (is (realm/record? (realm/compile T))))

(deftest simple-validation
  (is (record/is-a? T
                    (realm-validation/checking
                     (T f1 5 f2 "foo"))))
  (is (thrown? Exception
               (realm-validation/checking
                (T f1 "bar" f2 "foo")))))

(deftest extended-realm-structs
  (is (realm/record? (realm/compile ExtT)))

  (is (record/is-a? ExtT
                    (realm-validation/checking
                     (ExtT f1 5 f2 "foo" f3 :bar))))

  (is (thrown? Exception
               (realm-validation/checking
                (ExtT f1 5 f2 "foo" f3 "test")))))
