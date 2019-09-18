(ns intro-to-ec.heuristic-search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as pm]))

(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

(def depth-first-search
  {:get-next-node #(first (first %))
   :add-children #(reduce (fn [front child] (assoc front child (%1 child))) %2 %3)})

(def breadth-first-search
  {:get-next-node first
   :add-children assoc})

(def random-search
  {:get-next-node rand-nth
   :add-children concat})


(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

(defn search
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [frontier (pm/priority-map start-node (heuristic start-node))
         came-from {start-node :start-node}
         num-calls 0]
    (println num-calls ": " frontier)
    (println came-from)
    (let [current-node (get-next-node frontier)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-states
                    (make-children current-node) frontier (keys came-from))]
          (recur
           (add-children
            heuristic
            (rest frontier)
              kids)
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)))))))
