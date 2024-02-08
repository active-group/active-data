(ns active.data.benchmarks
  (:require [criterium.core :as c]
            [active.data.record :as r]))

(defmacro bench [what construct access modify]
  `(do (println "***" ~what "construct")
       (c/bench (~construct 42))

       (println "***" ~what "access")
       (let [v# (~construct 42)]
         (c/bench (~access v#)))

       (println "***" ~what "modify")
       (let [v# (~construct 42)]
         (c/bench (~modify v#)))))

(r/def-record ADRecord [r-a r-b r-c r-d r-e])
(def mk-ad (r/constructor ADRecord))

(def get-a r-a #_ (active.data.struct/accessor ADRecord r-a))

(defrecord JRecord [a b c d e])

(r/def-record Spec [r-a])
(def spec (r/constructor Spec))

(defn -main []
  (println "active.data.record create size 1")
  (c/bench ((fn [v] (spec v)) 42))
  
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
           (assoc v :b 10)))
  )
