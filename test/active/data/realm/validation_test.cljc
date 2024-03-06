(ns active.data.realm.validation-test
    (:require [active.data.realm :as realm]
              [active.data.realm.attach :as realm-attach #?@(:cljs [:include-macros true])]
              [active.data.realm.validation :as realm-validation #?@(:cljs [:include-macros true])]
              #?(:cljs [cljs.test :refer-macros (deftest is testing)])
              #?(:clj [clojure.test :refer (deftest is testing)])))

(realm-attach/defn twoify :- realm/integer
  [n :- realm/integer]
  (* n 2))

(deftest checking-checks
  (is (= 14 (twoify 7)))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (realm-validation/checking
                (twoify "7")))))

