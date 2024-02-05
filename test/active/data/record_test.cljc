(ns active.data.record-test
  (:require [active.data.record :as sut #?@(:cljs [:include-macros true])]
            [active.data.struct :as struct]
            [active.data.struct.validator :as validator]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

;; Focuses on what is different than in structs

(defn throws [t]
  #?(:clj (try (t) nil (catch Throwable e e))
     :cljs (try (t) nil (catch :default e e))))

(sut/def-record R
  [r-a r-b])

(t/deftest construction-test
  (t/is (some? (R r-a 42 r-b "foo")))

  (t/is (some? (throws #(R r-a 42)))
        "Cannot construct with partial fields")

  (t/testing "reflection"
    (let [v (R r-a 42 r-b "foo")]
      (t/is (= R (sut/record-of v))))

    (t/is (= #{r-a r-b} (sut/record-keys R))))
  
  (t/testing "predicates"
    (let [v (R r-a 42 r-b "foo")]
      (t/is (sut/is-a? R v))
      (t/is (sut/is-exactly-a? R v)))))

(t/deftest empty-test
  (sut/def-record Empty1 [])
  (sut/def-record Empty2 [])

  (t/is (not= Empty1 Empty2))

  (t/is (not= (Empty1) (Empty2))))

(t/deftest no-foreign-keys-test
  (t/is (some? (throws #(get (R r-a 42 r-b "foo") :bla))))
  
  (t/is (some? (throws #(assoc (R r-a 42 r-b "foo") :bla 1)))))

(t/deftest no-dissoc-keys-test
  (t/is (some? (throws #(dissoc (R r-a 42 r-b "foo") r-a)))))

(t/deftest printer-test
  (let [v (R r-a 42 r-b "foo")]
    ;; TODO: change how keys are printed here?
    
    (t/is (= "#active.data.record-test/R{~r-a 42, ~r-b \"foo\"}" (pr-str v)))

    ;; TODO: or just the record type name?
    (t/is (= "#active.data.record.Record{~r-a, ~r-b}" (pr-str R)))
    ))

(t/deftest extends-test
  (sut/def-record ExtR :extends R [r-c])

  (t/is (some? (ExtR r-a 42 r-b "foo" r-c :bla)))

  (t/testing "predicates"
    (let [v (ExtR r-a 42 r-b "foo" r-c :bla)]
      (t/is (sut/is-a? R v))
      (t/is (sut/is-extended-from? R v))

      (t/is (not (sut/is-exactly-a? R v)))
      (t/is (sut/is-exactly-a? ExtR v)))))

;; TODO
#_(t/deftest validator-test

  (sut/def-struct ValidatedT [vt-a vt-b])
  (sut/set-validator! ValidatedT (AllInt.))
  
  (let [valid (sut/struct-map ValidatedT vt-a 42 vt-b 21)]
    (t/testing "contruction checks for validity"
      (t/is (throws #(sut/struct-map ValidatedT vt-a :foo vt-b 21))))

    (t/testing "modification checks for validity"
      (t/is (throws #(assoc valid vt-a :foo)))
      (t/is (throws #(into valid {vt-a :foo})))
      (t/is (throws #(empty valid))))

    ;; TODO: not currently anymore
    #_(t/testing "has-keys? checks for validity"  
      (t/is (sut/has-keys? ValidatedT {vt-a 11 vt-b 22}))
      (t/is (not (sut/has-keys? ValidatedT {vt-a :foo vt-b :bar})))))
  )

