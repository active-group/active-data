(ns hooks.active-data
  (:require [clj-kondo.hooks-api :as api]
            [clojure.string :as str]
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
                            [maybe-base-name maybe-fields]
                            (cond (and (api/keyword-node? (first args))
                                       (= :extends
                                          (:k (first args)))
                                       (api/token-node? (second args))
                                       (api/vector-node? (third args)))
                                  [(second args) (third args)]

                                  (api/vector-node? (first args))
                                  [nil (first args)]

                                  :else
                                  [nil nil])

                            partitioned-field-vector (partition-fields-and-realms (:children maybe-fields))
                            field-names              (map #(if (vector? %)
                                                             (first %)
                                                             %)
                                                          partitioned-field-vector)
                            new-node
                            (apply as-do
                                   (api/list-node (list (api/token-node 'declare)
                                                        record-name))
                                   (map
                                    #(api/list-node (list (api/token-node 'declare)
                                                          %))
                                    field-names))]
                        ;; (pp/pprint (api/sexpr new-node))
                        new-node)))))
