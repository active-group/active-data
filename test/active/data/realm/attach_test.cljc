(ns active.data.realm.attach-test
  (:require [active.data.realm :as realm]
            [active.data.realm.attach :as realm-attach #?@(:cljs [:include-macros true])]
            #?(:cljs [cljs.test :refer-macros (deftest is testing)])
            #?(:clj [clojure.test :refer (deftest is testing)])))

(realm-attach/defn realm/integer
  twoify
  [n realm/integer]
  (* n 2))

(deftest defn-defines
  (is (= 14 (twoify 7))))
