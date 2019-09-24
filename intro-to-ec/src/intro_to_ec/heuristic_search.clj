(ns intro-to-ec.heuristic-search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as pm]))

(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

(def heuristic-search
  {:get-next-node #(first (first %))
   :add-children #(reduce (fn [front child] (assoc front child (%1 child nil nil))) %2 %3)})

(def a-star-search
  {:get-next-node #(first (first %))
   :add-children #(reduce (fn [front child] (assoc front child (+ (%1 child nil nil) %4))) %2 %3)})

(def double-search
    {:get-next-node #(first (first %))
     :add-children #(reduce (fn [front child] (assoc front child (+ (%1 child %2 %5) %5))) %3 %4)})

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

(defn search-hs
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [frontier (pm/priority-map start-node (heuristic start-node nil nil))
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
            (pop frontier)
              kids)
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)))))))
nil
(defn search-a-star
 [{:keys [get-next-node add-children]}
  {:keys [goal? make-children heuristic]}
  start-node max-calls]
 (loop [frontier (pm/priority-map start-node (heuristic start-node nil nil))
        cost-so-far {start-node 0}
        came-from {start-node :start-node}
        num-calls 0]
   (println num-calls ": " frontier)
   (println came-from)
   (println "Cost-so-far is: " cost-so-far)
   (let [current-node (get-next-node frontier)
         new-cost (+ (get cost-so-far current-node) 1)]
     (cond
       (goal? current-node) (generate-path came-from current-node)
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
          (reduce (fn [costs child] (assoc costs child (heuristic child nil nil))) cost-so-far kids)
          (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
          (inc num-calls)))))))

(defn search-doubles
 [{:keys [get-next-node add-children]}
  {:keys [goal? make-children heuristic]}
  start-node max-calls]
 (loop [frontier (pm/priority-map start-node (heuristic start-node :start-node 0))
        cost-so-far {start-node 0}
        came-from {start-node :start-node}
        num-calls 0]
   (println num-calls ": " frontier)
   (println came-from)
   (println "Cost-so-far is: " cost-so-far)
   (let [current-node (get-next-node frontier)
         new-cost (+ (get cost-so-far current-node) 1)]
     (cond
       (goal? current-node) (generate-path came-from current-node)
       (= num-calls max-calls) :max-calls-reached
       :else
       (let [kids (remove-previous-states
                   (make-children current-node) frontier (keys came-from))]
         (recur
          (add-children
           heuristic
            current-node
           (pop frontier)
             kids
             new-cost)
          (reduce (fn [costs child] (assoc costs child (heuristic child current-node new-cost))) cost-so-far kids)
          (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
          (inc num-calls)))))))
