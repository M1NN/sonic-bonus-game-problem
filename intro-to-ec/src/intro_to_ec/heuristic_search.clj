(ns intro-to-ec.heuristic-search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as pm]))

(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

(def a-star-search
  {:get-next-node #(first (first %))
   :add-children #(reduce (fn [front child] (assoc front child (+ (%1 child) %4))) %2 %3)})


(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) (val (first node)))))


(defn search-a-star
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [frontier (pm/priority-map start-node (heuristic start-node))
         cost-so-far {start-node 9}
         came-from {start-node :start-node}
         num-calls 0]
    (println "Number of calls so far is: " num-calls)
    (let [current-node (get-next-node frontier)
          new-cost (+ (get cost-so-far current-node) 1)]
      (println "Our current path: " (generate-path came-from current-node))
      (cond
        (goal? current-node) 
        (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-states
                    (make-children current-node) frontier (keys came-from))]
          (recur
           (add-children
            heuristic
            (pop frontier)
            kids
            new-cost)
           (reduce (fn [costs child] (assoc costs child (heuristic child))) cost-so-far kids)
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)
           ))))))


