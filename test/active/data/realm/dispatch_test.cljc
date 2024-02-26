(ns active.data.realm.dispatch-test
  (:require [active.data.realm :as realm]
            [active.data.realm.dispatch :as realm-dispatch #?@(:cljs [:include-macros true])]
            #?(:cljs [cljs.test :refer-macros (deftest is testing)])
            #?(:clj [clojure.test :refer (deftest is testing)])))

(def schema
  (realm/union
   realm/integer
   realm/string
   realm/keyword))

(deftest happy-test
  (is (= 1
         (realm-dispatch/union-case schema 17
                                    realm/integer 1
                                    realm/string 2
                                    realm/keyword 3)))
  (is (= 2
         (realm-dispatch/union-case schema "17"
                                    realm/integer 1
                                    realm/string 2
                                    realm/keyword 3)))
  (is (= 3
         (realm-dispatch/union-case schema :seventeen
                                    realm/integer 1
                                    realm/string 2
                                    realm/keyword 3))))

(deftest errors-test
  ;; No 'compile time checks' currently:
  #_(is (= :syntax-error
         (try
           (macroexpand '(active.data.realm.dispatch/union-case schema :foo
                           active.data.realm/integer 1
                           active.data.realm/keyword 2))
           (catch #?(:clj Throwable :cljs js/Error) e :syntax-error)))
        "Missing realm compile time error")

  (is (thrown? #?(:clj Exception :cljs js/Error)
               (realm-dispatch/union-case schema 5.5
                                          realm/integer 1
                                          realm/string 2
                                          realm/keyword 3))
      "Missing realm runtime error"))

