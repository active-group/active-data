(ns hooks.active-data
  (:require [clj-kondo.hooks-api :as api]
            [clojure.pprint :as pp]))

(defn- rewrite-list
  "rewrite children list of a list-node to a single new node."
  [expr f]
  (-> expr
      (update :node
              (fn [node]
                (if (api/list-node? node)
                  (let [cs (:children node)]
                    (f cs))
                  ;; just keep? or an error?
                  (do #_(assert false node) ;; TODO: proper error
                      node))))))

(defn- drop-docstring [name x & more]
  (if (api/string-node? x)
    (list* name more)
    (list* name (cons x more))))

(defn- as-do [& nodes]
  ;; multiple nodes in a 'do'
  (api/list-node (list* (api/token-node 'do) nodes)))

(defn- third [l]
  (first (nnext l)))

(defn- analyze-fields-and-realms [field-vector-children]
  (loop [exprs field-vector-children
         res {:names []
              :realms []}]
    (cond
      (empty? exprs) res

      (empty? (rest exprs))
      (-> res (update :names conj (first exprs)))

      (and (api/keyword-node? (second exprs))
           (= :- (:k (second exprs))))
      (recur (drop 3 exprs)
             (-> res
                 (update :names conj (first exprs))
                 (update :realms conj (third exprs))))

      :else
      (recur (rest exprs)
             (-> res
                 (update :names conj (first exprs)))))))

(defn def-record [expr]
  (-> expr
      (rewrite-list (fn [children]
                      ;; rewrite node to defn, removing :static
                      (let [[record-name & args] (apply drop-docstring (rest children))
                            [maybe-base-name fields]
                            (loop [args args
                                   res [nil nil]]
                              (cond
                                (empty? args)
                                res
                                
                                (and (api/keyword-node? (first args))
                                     (api/token-node? (second args)))
                                (if (= :extends (:k (first args)))
                                  (recur (drop 2 args)
                                         [(second args) (second res)])
                                  ;; drop other options
                                  (recur (drop 2 args) res))

                                (api/vector-node? (first args))
                                [(first res) (:children (first args))]
                                
                                :else ;; something is syntactically wrong
                                res))

                            {field-names :names field-realms :realms} (analyze-fields-and-realms fields)
                            
                            new-node
                            (apply as-do
                                   ;; reference the base record and realms used
                                   maybe-base-name
                                   (api/vector-node field-realms)
                                   ;; declare the record name (arities are complex)
                                   (api/list-node (list (api/token-node 'declare)
                                                        record-name))
                                   (map
                                    ;; define fields as 1 or 2 arity fns.
                                    #(api/list-node (list (api/token-node 'defn)
                                                          %
                                                          (api/list-node (list (api/vector-node (list (api/token-node '_r))) (api/token-node nil)))
                                                          (api/list-node (list (api/vector-node (list (api/token-node '_r) (api/token-node '_v))) (api/token-node nil)))))
                                    field-names))]
                        ;; (pp/pprint (api/sexpr new-node))
                        new-node)))))

(defn function [expr]
  ;; could do some more checking here, but just separate references from syntax for now.
  (-> expr
      (rewrite-list
       (fn [children]
         (let [get-vals (fn get-vals [nodes]
                          (->> nodes
                               (mapcat (fn [n]
                                         (cond
                                           (api/token-node? n) (case (:value n)
                                                                 (& ->) nil
                                                                 [n])
                                           (api/list-node? n) (get-vals (:children n))
                                           (api/vector-node? n) (get-vals (:children n))
                                           (api/map-node? n) (get-vals (:children n))
                                           :else nil)))))]
           (apply as-do (get-vals (rest children))))))))

(defn realm-defn [expr]
  (-> expr
      (rewrite-list
       (fn [children]
         ;; (a/defn foo :- realm/string "docstring" [bar :- realm/integer] baz)
         (let [fn-name (second children)
               {params :params body :body realms :realms}
               (loop [exprs (drop 2 children)
                      res {:params [] :body nil :realms []}]
                 (cond
                   (empty? exprs) res
                   
                   ;; return realm
                   (and (api/keyword-node? (first exprs))
                        (= :- (:k (first exprs))))
                   (recur (drop 2 exprs)
                          (-> res
                              (update :realms conj (second exprs))))

                   ;; docstring, ignore
                   (api/string-node? (first exprs))
                   (recur (rest exprs) res)
                   
                   ;; params
                   (api/vector-node? (first exprs))
                   (let [{params :names param-realms :realms} (analyze-fields-and-realms (:children (first exprs)))]
                     (-> res
                         (assoc :params params)
                         (update :realms concat param-realms)
                         (assoc :body (rest exprs))))))]
           (as-do
            (api/vector-node realms)
            (api/list-node (list* (api/token-node 'defn)
                                  fn-name
                                  (api/vector-node params)
                                  body))))))))
