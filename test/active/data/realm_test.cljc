(ns active.data.realm-test
  (:require [active.data.realm :as realm]
            #?(:cljs [cljs.test :as t])
            #?(:clj [clojure.test :refer :all]))
  #?(:cljs (:require-macros [cljs.test :refer (is deftest run-tests testing)])))

(deftest builtin-scalar-realms-test
  (is (= realm/int
         (realm/compile int)))
  (is (= realm/bigdec
         (realm/compile bigdec)))
  (is (= realm/float
         (realm/compile float)))
  (is (= realm/double
         (realm/compile double)))
  (is (= realm/keyword
         (realm/compile keyword)))
  (is (= realm/symbol
         (realm/compile symbol)))
  (is (= realm/string
         (realm/compile str))))

(deftest predicate-test
  (is (realm/predicate?
       (realm/compile integer?))))

(deftest seq-of-test
  (is (realm/seq-of?
       (realm/compile [int])))
  (is (= realm/int
         (realm/seq-of-realm-realm (realm/compile [int])))))

(deftest set-of-test
  (is (realm/set-of?
       (realm/compile #{int})))
  (is (= realm/int
         (realm/set-of-realm-realm (realm/compile #{int})))))

(deftest map-with-keys-test
  (is (realm/map-with-keys?
       (realm/compile {:a int
                       :b double}))))

(deftest map-of-test
  (is (realm/map-of?
       (realm/compile {int double}))))

(deftest tuple-test
  (is (realm/tuple?
       (realm/compile [int double]))))

(defrecord Pare [kar kdr])

(def pare-realm
  (realm/record "Pare"
                ->Pare
                (partial instance? Pare)
                [(realm/field "kar" realm/int :kar)
                 (realm/field "kdr" realm/double :kdr)]))

(deftest description-test
  (is (= "optional int"
         (realm/description (realm/optional realm/int))))
  (is (= "integer from 5 to 10"
         (realm/description (realm/integer-from-to 5 10))))
  (is (= "mixed of [int, float, double]"
         (realm/description (realm/mixed realm/int realm/float realm/double))))
  (is (= "enumeration of [1, 2, 3]"
         (realm/description (realm/enum 1 2 3))))
  (is (= "map with keys {:a -> int, :b -> double}"
         (realm/description (realm/map-with-keys {:a realm/int :b realm/double}))))
  (is (= "map of {int -> double}"
         (realm/description (realm/map-of realm/int realm/double))))
  (is (= "record Pare with fields kar from realm int, kdr from realm double"
         (realm/description pare-realm))))



                                          
                     
  
