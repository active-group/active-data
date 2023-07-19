(ns active.data.realm.validation-test
    (:require [active.data.realm :as realm]
              [active.data.realm.attach :as realm-attach]
              [active.data.realm.validation :as realm-validation]
              #?(:cljs [cljs.test :as t])
              #?(:clj [clojure.test :refer :all])))

(realm-attach/defn realm/int
  twoify
  [n realm/int]
  (* n 2))

(deftest checking-checks
  (is (= 14 (twoify 7)))
  (is (thrown? Exception
               (realm-validation/checking
                (twoify "7")))))

