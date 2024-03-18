(ns active.data.realm.attach-test
  (:require [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.realm.attach :as realm-attach #?@(:cljs [:include-macros true])]
            #?(:cljs [cljs.test :refer-macros (deftest is testing)])
            #?(:clj [clojure.test :refer (deftest is testing)])))

(realm-attach/defn twoify :- realm/integer
  [n :- realm/integer]
  (* n 2))

;; realms are optional
(realm-attach/defn test-fn [a b])

(deftest defn-defines-test
  (is (= 14 (twoify 7))))

#?(:clj
   (deftest fn-realm-test
     (is (realm-inspection/function? (realm-attach/fn-realm #'twoify)))))
