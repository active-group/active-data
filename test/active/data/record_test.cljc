(ns active.data.record-test
  (:require [active.data.record :as sut #?@(:cljs [:include-macros true])]
            [active.data.struct :as struct]
            [active.data.struct.key :as key]
            [active.data.struct.validator :as validator]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

;; Focuses on what is different than in structs

(defn throws [t]
  #?(:clj (try (t) nil (catch Throwable e e))
     :cljs (try (t) nil (catch :default e e))))

(sut/def-record R
  [r-a r-b])

(sut/def-record OtherR
  [or-a or-b])

;; TODO
#_#_(sut/def-struct ^:private PrivT [pt-a ^{:private false} pt-b])
(t/testing "private inheritance"
    (t/is (:private (meta #'PrivT)))
    (t/is (:private (meta #'pt-a)) "Privateness is inherited")
    (t/is (not (:private (meta #'pt-b))) "Privateness is inherited only as a default"))

(t/deftest optimization-test
  (t/is (key/optimized-for? r-a R))
  (t/is (not (key/optimized-for? r-a OtherR)))

  ;; Note: ClosedStructMap must also make use of that, but that cannot be tested extrinsicly.
  )

(t/deftest construction-test
  (t/is (some? (R r-a 42 r-b "foo")))

  (t/is (some? (throws #(R :bar 42)))
        "Cannot construct with foreign fields")

  (t/testing "predicates"
    (let [v (R r-a 42 r-b "foo")]
      (t/is (sut/is-a? R v))
      (t/is (sut/is-exactly-a? R v)))))

(t/deftest equality-test
  (t/is (= R R))
  
  ;; instances are equal
  (t/is (= (R r-a 42 r-b "foo") (R r-a 42 r-b "foo")))

  (t/is (= (R r-a 10 r-b "foo") (r-a (R r-a 42 r-b "foo") 10)))

  ;; not equal to maps
  (t/is (not= (R r-a 42 r-b "foo") {r-a 42 r-b "foo"})))

#?(:clj
   (t/deftest non-generative-clj-test
     (let [[t1 v1] (eval '(vector (active.data.record/def-record A [a])
                                  (A a :foo)))
           [t2 v2] (eval '(vector (active.data.record/def-record A [a])
                                  (A a :foo)))]

       ;; records and instances are equal
       (t/is (= t1 t2))
       
       (t/is (= v1 v2))

       ;; both values are exact instances of the "other" record
       (t/is (sut/is-exactly-a? t1 v2))
       (t/is (sut/is-exactly-a? t2 v1))
       )))

(t/deftest reflection-test
  (t/is (= R (sut/record-of (R r-a 42 r-b :foo))))

  (t/is (=  [r-a r-b] (sut/record-keys R)))

  (t/is (= 'active.data.record-test/R (sut/record-name R))))

(t/deftest keys-test
  (t/is (= 42 (r-a (R r-a 42 r-b "foo"))))
  (t/is (= "foo" (r-b (R r-a 42 r-b "foo"))))

  (t/is (= 11 (r-a (r-a (R r-a 42 r-b "foo") 11))))
  (t/is (= "bla" (r-b (r-b (R r-a 42 r-b "foo") "bla")))))

;; TODO: -> keys-test ?
#_(t/deftest keyed-map-test
  (t/testing "keys also work for hash-maps"
    (let [v {t-a 42
             t-b :foo}]
      (t/testing "access"
        (t/is (= 42 (t-a v)))
        (t/is (= :foo (get v t-b)))

        (t/is (= nil (t-b {})))
        (t/is (= nil (get {} t-b)))
        )

      (t/testing "update"
       (t/is (= {t-a 42 t-b :bar}
                (t-b v :bar)))
    
       (t/is (= {t-a 42 t-b :bar}
                (assoc v :t-b :bar))))))
  )

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
#_#_
(defrecord AllInt []
  validator/IMapValidator
  (-validate-field! [this changed-key changed-value]
    (when-not (int? changed-value)
      (throw (ex-info "Not an int" {:v changed-value}))))
  (-validate-map! [this m]
    nil))


(t/deftest validator-test

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

#_(sut/def-struct ExT
  :extends T
  [t-c])

#_(t/deftest extends-test
  (let [v (sut/struct-map ExT t-a 42 t-b :test t-c "xxx")]

    (t/is (sut/has-keys? ExT v))
    (t/is (sut/has-keys? T v))
    
    (t/is (= 10 (t-a (assoc v t-a 10))))
    (t/is (= "yyy" (t-c (assoc v t-c "yyy"))))

    ;; optimizations
    (t/is (key/optimized-for? t-a ExT))
    )

  ;; TODO: not anymore...?
  #_(t/testing "inherited validation"
    (let [inspect-v (fn [atom]
                      (reify validator/IMapValidator
                        (-validate-field! [this changed-key changed-value]
                          (swap! atom conj [:field changed-key changed-value]))
                        (-validate-map! [this m]
                          (swap! atom conj [:map m]))))
          base-validations (atom [])
          derived-validations (atom [])]
      
      (sut/def-struct VBase [vt-a])
      (sut/set-validator! VBase (inspect-v base-validations))

      (sut/def-struct VExt :extends VBase [vt-b])
      (sut/set-validator! VExt (inspect-v derived-validations))
  
      (let [valid (sut/struct-map VExt vt-a 42 vt-b 21)]
        (t/testing "contruction checks for validity"
            (t/is (= [[:field vt-a 42]
                      [:map valid]]
                     @base-validations))
            (t/is (= [[:field vt-a 42]
                      [:field vt-b 21]
                      [:map valid]]
                     @derived-validations)))

        (reset! base-validations [])
        (reset! derived-validations [])

        (t/testing "modification checks for validity"
          ;; of a base field:
          (assoc valid vt-a :foo)
          (t/is (= `[[:field ~vt-a :foo]
                     [:map {~vt-a :foo, ~vt-b 21}]]
                   @base-validations))
          ;; Note: validator of derived struct is also called for change in base field
          (t/is (= `[[:field ~vt-a :foo]
                     [:map {~vt-a :foo, ~vt-b 21}]]
                   @derived-validations))

          (reset! base-validations [])
          (reset! derived-validations [])

          ;; of a derived field:
          (assoc valid vt-b :bar)
          (t/is (empty? @base-validations))
          (t/is (= `[[:field ~vt-b :bar]
                     [:map {~vt-a 42, ~vt-b :bar}]]
                   @derived-validations)))

        (reset! base-validations [])
        (reset! derived-validations [])

        (t/testing "has-keys? does not check base validity if derived struct-map"
          ;; Note: although it is not an is-a? base, the base validity must have been checked on construction already.
          
          (t/is (sut/has-keys? VBase valid))
          (t/is (empty? @base-validations))
          )))
    )
  
  )