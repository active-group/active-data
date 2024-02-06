(ns active.data.realm.validation-test
    (:require [active.data.realm :as realm]
              [active.data.realm.attach :as realm-attach #?@(:cljs [:include-macros true])]
              [active.data.realm.validation :as realm-validation #?@(:cljs [:include-macros true])]
              #?(:cljs [cljs.test :refer-macros (deftest is testing)])
              #?(:clj [clojure.test :refer (deftest is testing)])))

(realm-attach/defn realm/int
  twoify
  [n realm/int]
  (* n 2))

(deftest checking-checks
  (is (= 14 (twoify 7)))
  (is (thrown? Exception
               (realm-validation/checking
                (twoify "7")))))

