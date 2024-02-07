(ns active.data.benchmarks
  (:require [criterium.core :as c]
            [active.data.record :as r]))

(defmacro bench [what construct access modify]
  `(do (println "***" ~what "construct")
       (c/bench (~construct 42))

       (let [v# (println "***" ~what "access")]
         (c/bench (~access v#)))

       (let [v# (println "***" ~what "modify")]
         (c/bench (~modify v#)))))

(r/def-record ADRecord [r-a r-b r-c r-d r-e])
(def mk-ad (r/constructor ADRecord))

(defrecord JRecord [a b c d e])

(defn -main []
  (bench "active.data.record"
         (fn [x] (mk-ad :bla x :c :d :e))
         (fn [v]
           (r-a v))
         (fn [v]
           (r-b v 10)))

  (bench "hash-maps"
         (fn [x]
           {:a :bla :b x :c :c :d :d :e :e})
         (fn [v]
           (:a v))
         (fn [v]
           (assoc v :b 10)))

  (bench "native record"
         (fn [x] (JRecord. :bla x :c :d :e))
         (fn [v]
           (:a v))
         (fn [v]
           (assoc v :a 10)))
  )
