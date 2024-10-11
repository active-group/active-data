(ns hooks.active-data
  (:require [clj-kondo.hooks-api :as api]
            ;; [clojure.pprint :as pp]
            ))

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

(defn- partition-fields-and-realms [field-vector-children]
  (->> field-vector-children
       (reduce (fn [v arg-node]
                 (cond
                   (= :-
                      (:k arg-node))
                   (conj (vec (butlast v)) [(last v) arg-node])

                   ;; TODO maybe we should check if arg-node is a realm here
                   (= :-
                      (:k (last (last v))))
                   (conj (vec (butlast v)) (conj (last v) arg-node))

                   :else
                   (conj v arg-node)))
               [])))

(defn- third [l]
  (first (nnext l)))

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

                            partitioned-field-vector (partition-fields-and-realms fields)
                            field-names              (map #(if (vector? %)
                                                             (first %)
                                                             %)
                                                          partitioned-field-vector)
                            new-node
                            (apply as-do
                                   ;; reference the base record
                                   maybe-base-name
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
