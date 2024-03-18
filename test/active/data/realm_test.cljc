(ns active.data.realm-test
  (:require [active.data.realm :as realm #?@(:cljs [:include-macros true])]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.struct :as struct #?@(:cljs [:include-macros true])]
            [active.data.record :as record #?@(:cljs [:include-macros true])]
            #?(:cljs [cljs.test :refer-macros (is deftest testing)])
            #?(:clj [clojure.test :refer (is deftest testing)])))

(deftest sequence-of-test
  (is (realm-inspection/sequence-of?
       (realm/compile [realm/integer])))
  (is (= realm/integer
         (realm-inspection/sequence-of-realm-realm (realm/compile [realm/integer])))))

(deftest set-of-test
  (is (realm-inspection/set-of?
       (realm/compile #{realm/integer})))
  (is (= realm/integer
         (realm-inspection/set-of-realm-realm (realm/compile #{realm/integer})))))

(deftest map-with-keys-test
  (is (realm-inspection/map-with-keys?
       (realm/compile {:a realm/integer
                       :b realm/real}))))

(deftest map-of-test
  (is (realm-inspection/map-of?
       (realm/compile {realm/integer realm/real}))))

(deftest tuple-test
  (is (realm-inspection/tuple?
       (realm/compile [realm/integer realm/real]))))

(deftest function-test
  (is (realm-inspection/function?
       (realm/function realm/integer realm/integer -> realm/boolean)))
  (is (realm-inspection/function?
       (realm/function realm/integer realm/integer & (realm/string) -> realm/boolean)))
  (is (realm-inspection/function?
       (realm/function realm/integer realm/integer & [realm/string realm/integer realm/boolean] -> realm/boolean)))
  (is (realm-inspection/function?
       (realm/function realm/integer realm/integer & {:a realm/string :b realm/integer :c realm/boolean} -> realm/boolean))))

(deftest function-cases-test
  (is (realm-inspection/function?
       (realm/function-cases
        (realm/function realm/integer realm/integer -> realm/boolean)
        (realm/function realm/integer realm/integer & (realm/string) -> realm/boolean)))))

(deftest delay-test
  (is (realm-inspection/delayed? (realm/delay (/ 1 0)))))

(defprotocol Indexed
  (index [x index]))

(defrecord Pare [kar kdr]
  Indexed
  (index [_ index]
    (case (int index)
      (0) kar
      (1) kdr)))

(def pare-realm
  (realm/record "Pare"
                ->Pare
                (partial instance? Pare)
                [(realm/field "kar" realm/integer :kar)
                 (realm/field "kdr" realm/real :kdr)]))

(record/def-record Rare [rar rdr])

(def nonempty-string-realm
  (realm/restricted realm/string
                    (fn [s]
                      (> (count s) 0))
                    "nonempty strings"))

(deftest description-test
  (is (= "optional integer"
         (realm-inspection/description (realm/optional realm/integer))))
  (is (= "real range [5, 10]"
         (realm-inspection/description (realm/real-range :in 5 10 :in))))
  (is (= "real range (5, 10]"
         (realm-inspection/description (realm/real-range :ex 5 10 :in))))
  (is (= "real range [5, 10)"
         (realm-inspection/description (realm/real-range :in 5 10 :ex))))
  (is (= "real range (5, 10)"
         (realm-inspection/description (realm/real-range :ex 5 10 :ex))))
  (is (= "real range (5, )"
         (realm-inspection/description (realm/real-range :ex 5))))
  (is (= "real range [5, )"
         (realm-inspection/description (realm/real-range :in 5))))
  (is (= "real range (, 5)"
         (realm-inspection/description (realm/real-range 5 :ex))))
  (is (= "real range (, 5]"
         (realm-inspection/description (realm/real-range 5 :in))))
  (is (= "integer from 5 to 10"
         (realm-inspection/description (realm/integer-from-to 5 10))))
  (is (= "integer from 5"
         (realm-inspection/description (realm/integer-from 5))))
  (is (= "integer to 10"
         (realm-inspection/description (realm/integer-to 10))))
  (is (= "union of [integer, real]"
         (realm-inspection/description (realm/union realm/integer realm/real))))
  (is (= "intersection of [integer, real]"
         (realm-inspection/description (realm/intersection realm/integer realm/real))))
  (is (= "intersection of [natural, integer, real]"
         (realm-inspection/description (realm/intersection realm/natural realm/integer realm/real))))
  (is (= "enumeration of [1, 2, 3]"
         (realm-inspection/description (realm/enum 1 2 3))))
  (is (= "map with keys {:a -> integer, :b -> real}"
         (realm-inspection/description (realm/map-with-keys {:a realm/integer :b realm/real}))))
  (is (= "map of {integer -> real}"
         (realm-inspection/description (realm/map-of realm/integer realm/real))))
  (is (= "map with tag :foo -> :bar"
         (realm-inspection/description (realm/map-with-tag :foo :bar))))
  (is (= "function (integer, integer) -> boolean"
         (realm-inspection/description (realm/function realm/integer realm/integer -> realm/boolean))))
  (is (= "function (integer, integer & sequence of string) -> boolean"
         (realm-inspection/description (realm/function realm/integer realm/integer & (realm/string) -> realm/boolean))))
  (is (= "function (integer, integer & tuple of (string, integer, boolean)) -> boolean"
         (realm-inspection/description (realm/function realm/integer realm/integer & [realm/string realm/integer realm/boolean] -> realm/boolean))))
  (is (= "function (integer, integer & map with keys {:a -> string, :b -> integer, :c -> boolean}) -> boolean"
         (realm-inspection/description (realm/function realm/integer realm/integer & {:a realm/string :b realm/integer :c realm/boolean} -> realm/boolean))))
  (is (= "function with cases function (integer, integer) -> boolean, function (integer, integer & sequence of string) -> boolean"
         (realm-inspection/description (realm/function-cases
                             (realm/function realm/integer realm/integer -> realm/boolean)
                             (realm/function realm/integer realm/integer & (realm/string) -> realm/boolean)))))
  (is (= "record Pare with fields kar from realm integer, kdr from realm real"
         (realm-inspection/description pare-realm)))
  (is (= "record unnamed-struct with fields :sar from realm any, :sdr from realm any"
         (realm-inspection/description (realm/struct->record-realm (struct/struct [:sar :sdr])))))
  (is (= "record active.data.realm-test/Rare with fields rar from realm any, rdr from realm any"
         (realm-inspection/description (realm/record->record-realm Rare))))
  (is (= "delayed realm"
         (realm-inspection/description (realm/delay (/ 1 0)))))
  (is (= "realm named :a: integer"
         (realm-inspection/description (realm/named :a realm/integer))))
  (is (= "string restricted to nonempty strings"
         (realm-inspection/description nonempty-string-realm))))


(deftest predicate-test
  (is ((realm-inspection/predicate realm/integer) 5))
  (is (not ((realm-inspection/predicate realm/integer) "5")))
  (is ((realm-inspection/predicate realm/real) 5.0))
  (is (not ((realm-inspection/predicate realm/real) :five)))
  (is ((realm-inspection/predicate realm/boolean) true))
  (is (not ((realm-inspection/predicate realm/boolean) "true")))
  (is ((realm-inspection/predicate realm/keyword) :keyword))
  (is (not ((realm-inspection/predicate realm/keyword) "keyword")))
  (is ((realm-inspection/predicate realm/string) "string"))
  (is (not ((realm-inspection/predicate realm/string) :string)))
  (is ((realm-inspection/predicate realm/uuid) #uuid "66a73374-6730-4a4b-a835-78e938293918"))
  (is (not ((realm-inspection/predicate realm/uuid) "66a73374-6730-4a4b-a835-78e938293918")))

  (is ((realm-inspection/predicate realm/any) "string"))

  (is ((realm-inspection/predicate (realm/from-predicate "ints" int?)) 5))
  (is (not ((realm-inspection/predicate (realm/from-predicate "ints" int?)) "5")))

  (is ((realm-inspection/predicate (realm/optional realm/integer)) 5))
  (is ((realm-inspection/predicate (realm/optional realm/integer)) nil))
  (is (not ((realm-inspection/predicate (realm/optional realm/integer)) "5")))

  (is ((realm-inspection/predicate (realm/integer-from-to 5 7)) 5))
  (is ((realm-inspection/predicate (realm/integer-from-to 5 7)) 6))
  (is ((realm-inspection/predicate (realm/integer-from-to 5 7)) 7))
  (is (not ((realm-inspection/predicate (realm/integer-from-to 5 7)) 4)))
  (is (not ((realm-inspection/predicate (realm/integer-from-to 5 7)) 8)))
  (is (not ((realm-inspection/predicate (realm/integer-from-to 5 7)) "5")))

  (is ((realm-inspection/predicate (realm/integer-from 5)) 5))
  (is ((realm-inspection/predicate (realm/integer-from 5)) 6))
  (is (not ((realm-inspection/predicate (realm/integer-from 5)) 4)))

  (is ((realm-inspection/predicate (realm/integer-to 5)) 5))
  (is (not ((realm-inspection/predicate (realm/integer-to 5)) 6)))
  (is ((realm-inspection/predicate (realm/integer-to 5)) 4))

  (is ((realm-inspection/predicate (realm/real-range :in 5 7 :in)) 5))
  (is ((realm-inspection/predicate (realm/real-range :in 5 7 :in)) 7))
  (is (not ((realm-inspection/predicate (realm/real-range :in 5 7 :in)) 4.9)))
  (is (not ((realm-inspection/predicate (realm/real-range :in 5 7 :in)) 7.1)))

  (is ((realm-inspection/predicate (realm/real-range :in 5 7 :ex)) 5))
  (is (not ((realm-inspection/predicate (realm/real-range :in 5 7 :ex)) 7)))
  (is (not ((realm-inspection/predicate (realm/real-range :in 5 7 :ex)) 4.9)))

  (is (not ((realm-inspection/predicate (realm/real-range :ex 5 7 :in)) 5)))
  (is ((realm-inspection/predicate (realm/real-range :ex 5 7 :in)) 7))
  (is (not ((realm-inspection/predicate (realm/real-range :ex 5 7 :in)) 7.1)))

  (is ((realm-inspection/predicate (realm/real-range :ex 5 7 :ex)) 6))
  (is (not ((realm-inspection/predicate (realm/real-range :ex 5 7 :ex)) 5)))
  (is (not ((realm-inspection/predicate (realm/real-range :ex 5 7 :ex)) 7)))

  (is ((realm-inspection/predicate (realm/real-range :ex 5)) 6))
  (is (not ((realm-inspection/predicate (realm/real-range :ex 5)) 5)))
  
  (is ((realm-inspection/predicate (realm/real-range :in 5)) 5))
  (is (not ((realm-inspection/predicate (realm/real-range :in 5)) 4.9)))

  (is ((realm-inspection/predicate (realm/real-range 5 :ex)) 4))
  (is (not ((realm-inspection/predicate (realm/real-range 5 :ex)) 5)))

  (is ((realm-inspection/predicate (realm/real-range 5 :in)) 5))
  (is (not ((realm-inspection/predicate (realm/real-range 5 :in)) 6)))

  (is (not ((realm-inspection/predicate (realm/real-range :in 5 7 :in)) "5")))
  (is (not ((realm-inspection/predicate (realm/real-range :in 5 7 :ex)) "5")))
  (is (not ((realm-inspection/predicate (realm/real-range :ex 5 7 :in)) "5")))
  (is (not ((realm-inspection/predicate (realm/real-range :ex 5 7 :ex)) "5")))
  
  (is ((realm-inspection/predicate (realm/union realm/integer realm/string)) 5))
  (is ((realm-inspection/predicate (realm/union realm/integer realm/string)) "5"))
  (is (not ((realm-inspection/predicate (realm/union realm/integer realm/string)) :five)))

  (is ((realm-inspection/predicate (realm/intersection realm/string nonempty-string-realm)) "foo"))
  (is (not ((realm-inspection/predicate (realm/intersection realm/string nonempty-string-realm)) "")))

  (is ((realm-inspection/predicate (realm/intersection realm/natural realm/integer realm/real)) 5))
  (is (not ((realm-inspection/predicate (realm/intersection realm/natural realm/integer realm/real)) -5)))

  (is ((realm-inspection/predicate (realm/enum 2 3 5)) 2))
  (is ((realm-inspection/predicate (realm/enum 2 3 5)) 3))
  (is ((realm-inspection/predicate (realm/enum 2 3 5)) 5))
  (is (not ((realm-inspection/predicate (realm/enum 2 3 5)) 7)))

  (is ((realm-inspection/predicate (realm/sequence-of realm/integer)) [1 2 3]))
  (is ((realm-inspection/predicate (realm/sequence-of realm/integer)) [:one :two :three]))
  (is (not ((realm-inspection/predicate (realm/sequence-of realm/integer)) 5)))
  (is (not ((realm-inspection/predicate (realm/sequence-of realm/integer)) #{5})))
  (is (not ((realm-inspection/predicate (realm/sequence-of realm/integer)) {5 2})))
  
  (is ((realm-inspection/predicate (realm/set-of realm/integer)) #{1 2 3}))
  (is ((realm-inspection/predicate (realm/set-of realm/integer)) #{:one :two :three}))
  (is (not ((realm-inspection/predicate (realm/set-of realm/integer)) [1 2 3])))

  (is ((realm-inspection/predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) {:foo 5 :bar "5"}))
  (is ((realm-inspection/predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) {:foo "5" :bar 5}))
  (is ((realm-inspection/predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) {:bla 5 :baz "5"}))
  (is (not ((realm-inspection/predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) 5)))
  (is (not ((realm-inspection/predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) [])))

  (is ((realm-inspection/predicate (realm/map-with-tag :foo :bar)) {:foo :bar :baz "5"}))
  (is (not ((realm-inspection/predicate (realm/map-with-tag :foo :bar)) {:foop :bar :baz "5"})))
  (is (not ((realm-inspection/predicate (realm/map-with-tag :foo :bar)) {:foo :baz :baz "5"})))
  (is (not ((realm-inspection/predicate (realm/map-with-tag :foo :bar)) 5)))
  (is (not ((realm-inspection/predicate (realm/map-with-tag :foo :bar)) {})))
  
  (is ((realm-inspection/predicate (realm/map-of realm/keyword realm/integer)) {}))
  (is ((realm-inspection/predicate (realm/map-of realm/keyword realm/integer)) {:foo 5}))
  (is ((realm-inspection/predicate (realm/map-of realm/keyword realm/integer)) {"foo" "5"}))
  (is (not ((realm-inspection/predicate (realm/map-of realm/keyword realm/integer)) [])))

  (is ((realm-inspection/predicate (realm/tuple realm/keyword realm/integer)) [:foo 5]))
  (is ((realm-inspection/predicate (realm/tuple realm/keyword realm/integer)) [5 :foo]))
  (is (not ((realm-inspection/predicate (realm/tuple realm/keyword realm/integer)) [5 :foo 12])))
  (is (not ((realm-inspection/predicate (realm/tuple realm/keyword realm/integer)) {5 :foo})))
  (is (not ((realm-inspection/predicate (realm/tuple realm/keyword realm/integer)) #{5 :foo})))

  (let [s (struct/struct [:sar :sdr])]
    (is ((realm-inspection/predicate (realm/struct->record-realm s)) (s :sar 1 :sdr 2)))
    (is (not ((realm-inspection/predicate (realm/struct->record-realm s)) 5))))

  (is ((realm-inspection/predicate (realm/record->record-realm Rare)) (Rare rar 1 rdr 2)))
  (is (not ((realm-inspection/predicate (realm/record->record-realm Rare)) 5)))
  
  (is ((realm-inspection/predicate (realm/function realm/integer realm/boolean -> realm/integer)) (fn [a b] a)))
  (is (not ((realm-inspection/predicate (realm/function realm/integer realm/boolean -> realm/integer)) 5)))
  
  (let [r (realm/tuple realm/keyword realm/integer)]
    (is ((realm-inspection/predicate (realm/delay r)) [:foo 5]))
    (is (not ((realm-inspection/predicate (realm/delay r)) 5))))

  (is ((realm-inspection/predicate (realm/named :a realm/integer)) 5))
  (is (not ((realm-inspection/predicate (realm/named :a realm/integer)) "5")))

  (is ((realm-inspection/predicate nonempty-string-realm) "foo"))
  (is (not ((realm-inspection/predicate nonempty-string-realm) "")))
  (is (not ((realm-inspection/predicate nonempty-string-realm) 5)))
  
  )
