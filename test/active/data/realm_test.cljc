(ns active.data.realm-test
  (:require [active.data.realm :as realm]
            [active.data.struct :refer [def-struct]]
            [active.data.struct :as struct]
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
  (is (= realm/boolean
         (realm/compile boolean)))
  (is (= realm/keyword
         (realm/compile keyword)))
  (is (= realm/symbol
         (realm/compile symbol)))
  (is (= realm/string
         (realm/compile str))))

(deftest predicate-test
  (is (realm/predicate?
       (realm/compile integer?))))

(deftest sequence-of-test
  (is (realm/sequence-of?
       (realm/compile [int])))
  (is (= realm/int
         (realm/sequence-of-realm-realm (realm/compile [int])))))

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

(deftest function-test
  (is (realm/function?
       (realm/function int int -> boolean)))
  (is (realm/function?
       (realm/function int int & (realm/string) -> boolean)))
  (is (realm/function?
       (realm/function int int & [realm/string int boolean] -> boolean)))
  (is (realm/function?
       (realm/function int int & {:a realm/string :b int :c boolean} -> boolean))))

(deftest function-cases-test
  (is (realm/function?
       (realm/function-cases
        (realm/function int int -> boolean)
        (realm/function int int & (realm/string) -> boolean)))))

(deftest delay-test
  (is (realm/delayed? (realm/delay (/ 1 0)))))

(defprotocol Indexed
  (index [x index]))

(defrecord Pare [kar kdr]
  Indexed
  (index [_ index]
    (case index
      (0) kar
      (1) kdr)))

(def pare-realm
  (realm/record "Pare"
                ->Pare
                (partial instance? Pare)
                [(realm/field "kar" realm/int :kar)
                 (realm/field "kdr" realm/double :kdr)]))

(def-struct Sare [sar sdr])

(deftest protocol-test
  (is (realm/protocol?
       (realm/compile Indexed))))

(deftest named-test
  (is (realm/named?
       (realm/compile :a))))

(def nonempty-string-realm
  (realm/restricted realm/string
                    (fn [s]
                      (> (count s) 0))
                    "nonempty strings"))

(deftest restricted-test
  (is (realm/restricted? nonempty-string-realm)))

(deftest description-test
  (is (= "optional int"
         (realm/description (realm/optional realm/int))))
  (is (= "integer from 5 to 10"
         (realm/description (realm/integer-from-to 5 10))))
  (is (= "union of [int, float, double]"
         (realm/description (realm/union realm/int realm/float realm/double))))
  (is (= "intersection of [int, float, double]"
         (realm/description (realm/intersection realm/int realm/float realm/double))))
  (is (= "enumeration of [1, 2, 3]"
         (realm/description (realm/enum 1 2 3))))
  (is (= "map with keys {:a -> int, :b -> double}"
         (realm/description (realm/map-with-keys {:a realm/int :b realm/double}))))
  (is (= "map of {int -> double}"
         (realm/description (realm/map-of realm/int realm/double))))
  (is (= "function (int, int) -> boolean"
         (realm/description (realm/function int int -> boolean))))
  (is (= "function (int, int & sequence of string) -> boolean"
         (realm/description (realm/function int int & (realm/string) -> boolean))))
  (is (= "function (int, int & tuple of (string, int, boolean)) -> boolean"
         (realm/description (realm/function int int & [realm/string int boolean] -> boolean))))
  (is (= "function (int, int & map with keys {:a -> string, :b -> int, :c -> boolean}) -> boolean"
         (realm/description (realm/function int int & {:a realm/string :b int :c boolean} -> boolean))))
  (is (= "function with cases function (int, int) -> boolean, function (int, int & sequence of string) -> boolean"
         (realm/description (realm/function-cases
                             (realm/function int int -> boolean)
                             (realm/function int int & (realm/string) -> boolean)))))
  (is (= "record Pare with fields kar from realm int, kdr from realm double"
         (realm/description pare-realm)))
  (is (= "record active.data.realm-test/Sare with fields sar from realm any, sdr from realm any"
         (realm/description (realm/compile Sare))))
  (is (= "delayed realm"
         (realm/description (realm/delay (/ 1 0)))))
  (is (= "realm for protocol #'active.data.realm-test/Indexed"
         (realm/description (realm/protocol Indexed))))
  (is (= "realm named :a: int"
         (realm/description (realm/named :a int))))
  (is (= "string restricted to nonempty strings"
         (realm/description nonempty-string-realm))))
      

(deftest shallow-predicate-test
  (is ((realm/shallow-predicate realm/int) 5))
  (is (not ((realm/shallow-predicate realm/int) "5")))
  (is ((realm/shallow-predicate realm/bigdec) (bigdec 5)))
  (is (not ((realm/shallow-predicate realm/bigdec) "5")))
  (is ((realm/shallow-predicate realm/float) 5.0))
  (is (not ((realm/shallow-predicate realm/float) 5)))
  (is ((realm/shallow-predicate realm/double) 5.0))
  (is (not ((realm/shallow-predicate realm/double) 5)))
  (is ((realm/shallow-predicate realm/boolean) true))
  (is (not ((realm/shallow-predicate realm/boolean) "true")))
  (is ((realm/shallow-predicate realm/keyword) :keyword))
  (is (not ((realm/shallow-predicate realm/double) "keyword")))
  (is ((realm/shallow-predicate realm/string) "string"))
  (is (not ((realm/shallow-predicate realm/double) :string)))

  (is ((realm/shallow-predicate realm/any) "string"))

  (is ((realm/shallow-predicate (realm/predicate "ints" int?)) 5))
  (is (not ((realm/shallow-predicate (realm/predicate "ints" int?)) "5")))

  (is ((realm/shallow-predicate (realm/optional realm/int)) 5))
  (is ((realm/shallow-predicate (realm/optional realm/int)) nil))
  (is (not ((realm/shallow-predicate (realm/optional realm/int)) "5")))

  (is ((realm/shallow-predicate (realm/integer-from-to 5 7)) 5))
  (is ((realm/shallow-predicate (realm/integer-from-to 5 7)) 6))
  (is ((realm/shallow-predicate (realm/integer-from-to 5 7)) 7))
  (is (not ((realm/shallow-predicate (realm/integer-from-to 5 7)) 4)))
  (is (not ((realm/shallow-predicate (realm/integer-from-to 5 7)) 8)))
  (is (not ((realm/shallow-predicate (realm/integer-from-to 5 7)) "5")))

  (is ((realm/shallow-predicate (realm/union realm/int realm/string)) 5))
  (is ((realm/shallow-predicate (realm/union realm/int realm/string)) "5"))
  (is (not ((realm/shallow-predicate (realm/union realm/int realm/string)) :five)))

  (is ((realm/shallow-predicate (realm/intersection realm/string nonempty-string-realm)) "foo"))
  (is (not ((realm/shallow-predicate (realm/intersection realm/string nonempty-string-realm)) "")))

  (is ((realm/shallow-predicate (realm/enum 2 3 5)) 2))
  (is ((realm/shallow-predicate (realm/enum 2 3 5)) 3))
  (is ((realm/shallow-predicate (realm/enum 2 3 5)) 5))
  (is (not ((realm/shallow-predicate (realm/enum 2 3 5)) 7)))

  (is ((realm/shallow-predicate (realm/sequence-of realm/int)) [1 2 3]))
  (is ((realm/shallow-predicate (realm/sequence-of realm/int)) [:one :two :three]))
  (is (not ((realm/shallow-predicate (realm/sequence-of realm/int)) 5)))
  (is (not ((realm/shallow-predicate (realm/sequence-of realm/int)) #{5})))
  (is (not ((realm/shallow-predicate (realm/sequence-of realm/int)) {5 2})))
  
  (is ((realm/shallow-predicate (realm/set-of realm/int)) #{1 2 3}))
  (is ((realm/shallow-predicate (realm/set-of realm/int)) #{:one :two :three}))
  (is (not ((realm/shallow-predicate (realm/set-of realm/int)) [1 2 3])))

  (is ((realm/shallow-predicate (realm/map-with-keys {:foo realm/int :bar realm/string})) {:foo 5 :bar "5"}))
  (is ((realm/shallow-predicate (realm/map-with-keys {:foo realm/int :bar realm/string})) {:foo "5" :bar 5}))
  (is ((realm/shallow-predicate (realm/map-with-keys {:foo realm/int :bar realm/string})) {:bla 5 :baz "5"}))
  (is (not ((realm/shallow-predicate (realm/map-with-keys {:foo realm/int :bar realm/string})) 5)))
  (is (not ((realm/shallow-predicate (realm/map-with-keys {:foo realm/int :bar realm/string})) [])))

  (is ((realm/shallow-predicate (realm/map-of realm/keyword realm/int)) {}))
  (is ((realm/shallow-predicate (realm/map-of realm/keyword realm/int)) {:foo 5}))
  (is ((realm/shallow-predicate (realm/map-of realm/keyword realm/int)) {"foo" "5"}))
  (is (not ((realm/shallow-predicate (realm/map-of realm/keyword realm/int)) [])))

  (is ((realm/shallow-predicate (realm/tuple realm/keyword realm/int)) [:foo 5]))
  (is ((realm/shallow-predicate (realm/tuple realm/keyword realm/int)) [5 :foo]))
  (is (not ((realm/shallow-predicate (realm/tuple realm/keyword realm/int)) [5 :foo 12])))
  (is (not ((realm/shallow-predicate (realm/tuple realm/keyword realm/int)) {5 :foo})))
  (is (not ((realm/shallow-predicate (realm/tuple realm/keyword realm/int)) #{5 :foo})))

  (is ((realm/shallow-predicate (realm/compile Sare)) (struct/struct-map Sare sar 1 sdr 2)))
  (is (not ((realm/shallow-predicate (realm/compile Sare)) 5)))

  (is ((realm/shallow-predicate (realm/function int boolean -> int)) (fn [a b] a)))
  (is (not ((realm/shallow-predicate (realm/function int boolean -> int)) 5)))
  
  (is ((realm/shallow-predicate (realm/delay Sare)) (struct/struct-map Sare sar 1 sdr 2)))
  (is (not ((realm/shallow-predicate (realm/delay Sare)) 5)))

  (is ((realm/shallow-predicate (realm/named :a int)) 5))
  (is (not ((realm/shallow-predicate (realm/named :a int)) "5")))

  (is ((realm/shallow-predicate nonempty-string-realm) "foo"))
  (is (not ((realm/shallow-predicate nonempty-string-realm) "")))
  (is (not ((realm/shallow-predicate nonempty-string-realm) 5)))
  
  )
  
  
