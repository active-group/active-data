(ns active.data.record-test
  (:require [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.realm.validation :as realm-validation]
            [active.data.raw-record :as record #?@(:cljs [:include-macros true])]
            [active.data.record :as sut #?@(:cljs [:include-macros true])]
            #?(:cljs [cljs.test :refer-macros (is deftest testing)])
            #?(:clj [clojure.test :refer (is deftest testing)])))

;; Note: more detailed tests in raw-record-test; this just adds tests for the optional realms.

(sut/def-record T
  [f1 :- realm/integer,
   f2 :- realm/string])

;; realms are optional
(sut/def-record OptT
  [opt1,
   opt2 :- realm/string])

(sut/def-record ExtT
  :extends T
  [f3 :- realm/keyword])

(deftest realm
  (is (realm-inspection/record? (realm/compile T))))

(deftest simple-validation
  (is (record/is-a? T
                    (realm-validation/checking
                     (T f1 5 f2 "foo"))))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (realm-validation/checking
                (T f1 "bar" f2 "foo")))))

(deftest extended-realm-structs
  (is (realm-inspection/record? (realm/compile ExtT)))
  
  (is (some? (realm-validation/checking
              (ExtT f1 5 f2 "foo" f3 :bar))))

  (is (some? (ExtT f1 5 f2 "foo" f3 "wrong")) "can be created without checking")
  
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (realm-validation/checking
                (ExtT f1 5 f2 "foo" f3 "wrong")))
      "Validates own fields")

  (is (some? (ExtT f1 :wrong f2 "foo" f3 :bar)) "can be created without checking")
  
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (realm-validation/checking
                (ExtT f1 :wrong f2 "foo" f3 :bar)))
      "Validates fields 'inherited' fields too")
  )
