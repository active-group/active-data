(ns active.data.benchmarks
  (:require [criterium.core :as c]
            [active.data.record :as r]))

(defmacro bench [what construct access modify]
  `(do (println "***" ~what "construct")
       (c/bench (~construct))

       (let [v# (println "***" ~what "access")]
         (c/bench (~access v#)))

       (let [v# (println "***" ~what "modify")]
         (c/bench (~modify v#)))))

(r/def-record ADRecord [r-a r-b])
(def mk-ad (r/constructor ADRecord))

(defrecord JRecord [a b])

(defn -main []
  (bench "active.data.record"
         (fn [] (mk-ad :bla 42))
         (fn [v]
           (r-a v))
         (fn [v]
           (r-b v 10)))

  (bench "hash-maps"
         (fn []
           ;; Note: {:a :bla :b 42} about 100 times faster - because it's a compile time constant?
           ;; {:a :bla :b 42}
           (hash-map :a :bla :b 42))
         (fn [v]
           (:a v))
         (fn [v]
           (assoc v :b 10)))

  (bench "native record"
         (fn [] (JRecord. :bla 42))
         (fn [v]
           (:a v))
         (fn [v]
           (assoc v :a 10)))
  )
