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
  (clj-kondo/run! {:lint [(as-tempfile forms)]
                   :cache false
                   ;; :debug true
                   :config-dir (io/file "resources/clj-kondo.exports/de.active-group/active-data/")}))

(defn unresolved-symbols [result]
  (->> (:findings result)
       (filter #(= :unresolved-symbol (:type %)))
       (map :message)))

(deftest base-test
  (testing "test the utils actually work"
    (is (not (empty? (-> (lint '((ns test.namespace)
                                 (def foo bar)))
                         (unresolved-symbols)))))))

#_(deftest def-record-test
  (testing "plain record"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.record :as r]))
                                            (r/def-record Foo [foo-a foo-b])))))))

  (testing "record with realms"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace2 (:require [active.data.record :as r]
                                                                          [active.data.realm :as realm]))
                                            (r/def-record Foo [foo-a :- realm/string foo-b])))))))

  (testing "record with realms and options"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace3 (:require [active.data.record :as r]
                                                                          [active.data.realm :as realm]))
                                            (r/def-record Base [base-a])
                                            (r/def-record Foo :extends Base
                                              [foo-a foo-b  :- realm/string]))))))))

#_(deftest defn-attach-test
  (testing "plain defn"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm.attach :as a]))
                                            (a/defn foo [bar] bar)))))))

  (testing "defn with realms"
    (is (empty? (unresolved-symbols (lint '((ns test.namespace1 (:require [active.data.realm.attach :as a]
                                                                          [active.data.realm :as realm]))
                                            (a/defn foo :- realm/string [bar :- realm/integer] bar))))))))

#_(deftest function-test
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
