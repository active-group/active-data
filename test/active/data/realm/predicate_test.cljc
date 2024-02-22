(ns active.data.realm.predicate-test
  (:require [active.data.realm :as realm]
            [active.data.realm.predicate :as realm-predicate]
            [active.data.struct :as struct #?@(:cljs [:include-macros true])]
            [active.data.record :as record #?@(:cljs [:include-macros true])]
            #?(:cljs [cljs.test :refer-macros (deftest is testing)])
            #?(:clj [clojure.test :refer (deftest is testing)])))

(def nonempty-string-realm
  (realm/restricted realm/string
                    (fn [s]
                      (> (count s) 0))
                    "nonempty strings"))

(record/def-record Rare [rar rdr])

(deftest shallow-predicate-test
  (is ((realm-predicate/shallow-predicate realm/integer) 5))
  (is (not ((realm-predicate/shallow-predicate realm/integer) "5")))
  (is ((realm-predicate/shallow-predicate realm/real) 5.0))
  (is (not ((realm-predicate/shallow-predicate realm/real) :five)))
  (is ((realm-predicate/shallow-predicate realm/boolean) true))
  (is (not ((realm-predicate/shallow-predicate realm/boolean) "true")))
  (is ((realm-predicate/shallow-predicate realm/keyword) :keyword))
  (is (not ((realm-predicate/shallow-predicate realm/real) "keyword")))
  (is ((realm-predicate/shallow-predicate realm/string) "string"))
  (is (not ((realm-predicate/shallow-predicate realm/real) :string)))

  (is ((realm-predicate/shallow-predicate realm/any) "string"))

  (is ((realm-predicate/shallow-predicate (realm/predicate "ints" int?)) 5))
  (is (not ((realm-predicate/shallow-predicate (realm/predicate "ints" int?)) "5")))

  (is ((realm-predicate/shallow-predicate (realm/optional realm/integer)) 5))
  (is ((realm-predicate/shallow-predicate (realm/optional realm/integer)) nil))
  (is (not ((realm-predicate/shallow-predicate (realm/optional realm/integer)) "5")))

  (is ((realm-predicate/shallow-predicate (realm/integer-from-to 5 7)) 5))
  (is ((realm-predicate/shallow-predicate (realm/integer-from-to 5 7)) 6))
  (is ((realm-predicate/shallow-predicate (realm/integer-from-to 5 7)) 7))
  (is (not ((realm-predicate/shallow-predicate (realm/integer-from-to 5 7)) 4)))
  (is (not ((realm-predicate/shallow-predicate (realm/integer-from-to 5 7)) 8)))
  (is (not ((realm-predicate/shallow-predicate (realm/integer-from-to 5 7)) "5")))

  (is ((realm-predicate/shallow-predicate (realm/union realm/integer realm/string)) 5))
  (is ((realm-predicate/shallow-predicate (realm/union realm/integer realm/string)) "5"))
  (is (not ((realm-predicate/shallow-predicate (realm/union realm/integer realm/string)) :five)))

  (is ((realm-predicate/shallow-predicate (realm/intersection realm/string nonempty-string-realm)) "foo"))
  (is (not ((realm-predicate/shallow-predicate (realm/intersection realm/string nonempty-string-realm)) "")))

  (is ((realm-predicate/shallow-predicate (realm/enum 2 3 5)) 2))
  (is ((realm-predicate/shallow-predicate (realm/enum 2 3 5)) 3))
  (is ((realm-predicate/shallow-predicate (realm/enum 2 3 5)) 5))
  (is (not ((realm-predicate/shallow-predicate (realm/enum 2 3 5)) 7)))

  (is ((realm-predicate/shallow-predicate (realm/sequence-of realm/integer)) [1 2 3]))
  (is ((realm-predicate/shallow-predicate (realm/sequence-of realm/integer)) [:one :two :three]))
  (is (not ((realm-predicate/shallow-predicate (realm/sequence-of realm/integer)) 5)))
  (is (not ((realm-predicate/shallow-predicate (realm/sequence-of realm/integer)) #{5})))
  (is (not ((realm-predicate/shallow-predicate (realm/sequence-of realm/integer)) {5 2})))
  
  (is ((realm-predicate/shallow-predicate (realm/set-of realm/integer)) #{1 2 3}))
  (is ((realm-predicate/shallow-predicate (realm/set-of realm/integer)) #{:one :two :three}))
  (is (not ((realm-predicate/shallow-predicate (realm/set-of realm/integer)) [1 2 3])))

  (is ((realm-predicate/shallow-predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) {:foo 5 :bar "5"}))
  (is ((realm-predicate/shallow-predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) {:foo "5" :bar 5}))
  (is ((realm-predicate/shallow-predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) {:bla 5 :baz "5"}))
  (is (not ((realm-predicate/shallow-predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) 5)))
  (is (not ((realm-predicate/shallow-predicate (realm/map-with-keys {:foo realm/integer :bar realm/string})) [])))

  (is ((realm-predicate/shallow-predicate (realm/map-of realm/keyword realm/integer)) {}))
  (is ((realm-predicate/shallow-predicate (realm/map-of realm/keyword realm/integer)) {:foo 5}))
  (is ((realm-predicate/shallow-predicate (realm/map-of realm/keyword realm/integer)) {"foo" "5"}))
  (is (not ((realm-predicate/shallow-predicate (realm/map-of realm/keyword realm/integer)) [])))

  (is ((realm-predicate/shallow-predicate (realm/tuple realm/keyword realm/integer)) [:foo 5]))
  (is ((realm-predicate/shallow-predicate (realm/tuple realm/keyword realm/integer)) [5 :foo]))
  (is (not ((realm-predicate/shallow-predicate (realm/tuple realm/keyword realm/integer)) [5 :foo 12])))
  (is (not ((realm-predicate/shallow-predicate (realm/tuple realm/keyword realm/integer)) {5 :foo})))
  (is (not ((realm-predicate/shallow-predicate (realm/tuple realm/keyword realm/integer)) #{5 :foo})))

  (let [s (struct/struct [:sar :sdr])]
    (is ((realm-predicate/shallow-predicate (realm/struct->record-realm s)) (s :sar 1 :sdr 2)))
    (is (not ((realm-predicate/shallow-predicate (realm/struct->record-realm s)) 5))))

  (is ((realm-predicate/shallow-predicate (realm/record->record-realm Rare)) (Rare rar 1 rdr 2)))
  (is (not ((realm-predicate/shallow-predicate (realm/record->record-realm Rare)) 5)))
  
  (is ((realm-predicate/shallow-predicate (realm/function realm/integer realm/boolean -> realm/integer)) (fn [a b] a)))
  (is (not ((realm-predicate/shallow-predicate (realm/function realm/integer realm/boolean -> realm/integer)) 5)))
  
  (let [r (realm/tuple realm/keyword realm/integer)]
    (is ((realm-predicate/shallow-predicate (realm/delay r)) [:foo 5]))
    (is (not ((realm-predicate/shallow-predicate (realm/delay r)) 5))))

  (is ((realm-predicate/shallow-predicate (realm/named :a realm/integer)) 5))
  (is (not ((realm-predicate/shallow-predicate (realm/named :a realm/integer)) "5")))

  (is ((realm-predicate/shallow-predicate nonempty-string-realm) "foo"))
  (is (not ((realm-predicate/shallow-predicate nonempty-string-realm) "")))
  (is (not ((realm-predicate/shallow-predicate nonempty-string-realm) 5)))
  
  )
