(ns active.data.clj-kondo-test
  "Tests that the clj-kondo config in
  resources/clj-kondo.exports/de.active-group/active-data/ does what
  it is supposed to do."
  (:require [clj-kondo.core :as clj-kondo]
            [clojure.java.io :as io]
            [clojure.test :refer (is deftest testing)]))

(defn as-tempfile [forms]
  (let [f (java.io.File/createTempFile "active.data.clj_kondo_test." ".clj")]
    (with-open [s (io/output-stream f)]
      (binding [*out* (io/writer s)]
        (doseq [form forms]
          (println (pr-str form)))))
    f))

(defn lint [forms]
  (clj-kondo/run! {:lint         [(as-tempfile forms)]
                   :copy-configs false
                   :cache        false
                   ;; :debug true
                   :config-dir   "resources/clj-kondo.exports/de.active-group/active-data/"}))

(defn other-findings [result]
  (->> (:findings result)
       (remove #(= :unresolved-symbol (:type %)))))

(defn unresolved-symbols [result]
  (->> (:findings result)
       (filter #(= :unresolved-symbol (:type %)))
       (map :message)))

(deftest base-test
  (testing "test the utils actually work"
    (is (not-empty (-> (lint '((ns test.namespace)
                               (def foo bar)
                               foo
                               bar))
                       (unresolved-symbols))))))

(deftest def-record-test
  (testing "plain record has no unresolved-symbols"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.record :as r]))
                                            (r/def-record Foo [foo-a foo-b])
                                            Foo
                                            foo-a
                                            foo-b))))))

  (testing "record with realms"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace2 (:require [active.data.record :as r]
                                                                          [active.data.realm :as realm]))
                                            (r/def-record Foo [foo-a :- realm/string
                                                               foo-b :- realm/string
                                                               foo-c
                                                               foo-d :- realm/string
                                                               foo-e])))))))

  (testing "record with realms and options"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace3 (:require [active.data.record :as r]
                                                                          [active.data.realm :as realm]))
                                            (r/def-record Base [base-a])
                                            (r/def-record Foo
                                              :extends Base
                                              :future-option nil
                                              [foo-a foo-b :- realm/string]))))))
    (is (= 1 (count (unresolved-symbols (lint '((ns test.namespace4 (:require [active.data.record :as r]))
                                                (r/def-record Foo
                                                  :extends undefine_Base
                                                  [foo-a])))))))))

(deftest defn-attach-test
  (testing "plain defn"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm.attach :as a]))
                                            (a/defn foo [bar] bar))))))
    (is (= 1 (count (unresolved-symbols (lint '((ns test.namespace11 (:require [active.data.realm.attach :as a]))
                                                (a/defn foo [bar] baz)))))))
    (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm.attach :as a]))
                                            (a/defn foo "docstring" [bar] bar)))))))

  (testing "defn with realms"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm.attach :as a]
                                                                          [active.data.realm :as realm]))
                                            (a/defn foo :- realm/string [bar :- realm/integer] bar))))))
    (is (= 1 (count (unresolved-symbols (lint '((ns test.namespace10 (:require [active.data.realm.attach :as a]
                                                                              [active.data.realm :as realm]))
                                                (a/defn foo :- realm/string [bar :- realm/integer] baz)))))))))

(deftest function-test
  (testing "positive"
    (testing "simple function"
      (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm :as realm]))
                                              (realm/function realm/string -> realm/string)))))))
    (testing "function with rest args"
      (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm :as realm]))
                                              (realm/function realm/string & (realm/string) -> realm/string)))))))
    (testing "function with optional args"
      (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm :as realm]))
                                              (realm/function realm/string & [realm/string realm/string] -> realm/string)))))))
    (testing "function with optional keyword args"
      (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm :as realm]))
                                              (realm/function realm/string & {:a realm/string} -> realm/string))))))))
  (testing "negative"
    (testing "simple function"
      (is (= 2 (count (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm :as realm]))
                                                  (realm/function realm_in -> realm_out))))))))
    (testing "function with rest args"
      (is (= 1 (count (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm :as realm]))
                                                  (realm/function realm/string & (realm_string) -> realm/string))))))))
    (testing "function with optional args"
      (is (= 1 (count (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm :as realm]))
                                                  (realm/function realm/string & [realm_string realm/string] -> realm/string))))))))
    (testing "function with optional keyword args"
      (is (= 1 (count (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm :as realm]))
                                                  (realm/function realm/string & {:a realm_string} -> realm/string))))))))))

(deftest re-export-test
  (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm :as realm]
                                                                        [active.data.internal.export :refer [re-export]]))
                                          (re-export realm/integer-from-to
                                                     realm/integer-from)
                                          integer-from-to
                                          integer-from)))))
  ;; these don't work; I guess clj-kondo doesn't really check namespaced references in the same way?
  #_(is (empty? (unresolved-symbols (lint '((ns test.namespace2 (:require [active.data.realm.inspection :as i]))
                                            i/metadata
                                            i/optional-realm)))))
  #_(is (not (empty? (unresolved-symbols (lint '((ns test.namespace2 (:require [active.data.realm.inspection :as i]))
                                                 i/metadataxx)))))))
