(ns active.data.realm.attach-test
  (:require [active.data.realm :as realm]
            [active.data.realm.attach :as realm-attach]
            #?(:cljs [cljs.test :as t])
            #?(:clj [clojure.test :refer :all])))

(realm-attach/defn realm/int
  twoify
  [n realm/int]
  (* n 2))

(deftest defn-defines
  (is (= 14 (twoify 7))))
