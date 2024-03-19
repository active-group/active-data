(defproject de.active-group/active-data "0.1.0-SNAPSHOT"
  :description "Active Data: Support data modelling in Clojure"
  :url "https://github.com/active-group/active-data"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [prismatic/schema "1.4.1"]]

  :profiles {:test {:dependencies [[criterium "0.4.6"]]}}

  :global-vars {*warn-on-reflection* true}
  
  :aliases {"benchmarks" ["with-profiles" "+test," "run" "-m" active.data.benchmarks]}

  ;; Note: run ClojureScript tests with > npm run test

  :codox {:metadata {:doc/format :markdown}
          :source-uri "https://github.com/active-group/active-data/blob/{version}/{filepath}#L{line}"}
  )
