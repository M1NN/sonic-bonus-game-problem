(ns intro-to-ec.sonic-bonus-game-problem)

(defn origin-goal?
  "A goal checking function that checks if the current set of red-balls
   is equal to the blue-ball-set"
  [blue-ball-set position]
  (empty? (clojure.set/difference blue-ball-set
                                  (clojure.set/intersection blue-ball-set (val (second position))))))

;; The possible moves in this lattice world. Each
;; move is represented by a vector indicating the
;; change in both x and y coordinates associated
;; with this move.
(def up    [0  1])
(def down  [0 -1])
(def left  [-1  0])
(def right [1  0])
(def all-moves [up down left right])

(defn apply-move
  "Apply a move to a given position, yielding the new position
   If the new position is in the blue-ball set, add it to the red-ball-set"
  [blue-ball-set position move]
  {:postiton (vec (map + (val (first position)) move))
   :red-balls (if (contains? blue-ball-set (vec (map + (val (first position)) move))) 
                (conj (val (second position)) (vec (map + (val (first position)) move))) (val (second position)))})

(defn legal-coordinate
  "Limit our search to the space where the given coordinate
   is in the range [0, max-range)."
  [min-range max-range x]
  (and (>= x min-range) (< x max-range)))

(defn legal-state
  "Return true if both coordinates are legal and this position isn't
   in the 'red-ball' set."
  [min-range max-range red-ball-set position]
  (and (every? (partial legal-coordinate min-range max-range) (val (first position)))
       (not (contains? red-ball-set (val (first position))))))

(defn grid-children
  "Generate a list of all the possible child states
   reachable from the given initial position."
  [min-range max-range blue-ball-set position]
  (filter (partial legal-state min-range max-range (val (second position)))
          (map (partial apply-move blue-ball-set position) all-moves)))

(defn blue-left
  "Our heuristic for this problem, returns the amount of blue-balls
   minus the amount of red-balls if the position is in the blue-ball-set
   otherwise return the count of blue-balls"
  [blue-ball-set position]
  (if (contains? blue-ball-set (val (first position)))
    (- (count blue-ball-set) (count (val (second position)))) (count blue-ball-set)))

(def min-range -2)
(def max-range 3)
(def no-balls #{})
(def blue-3x3
  (set
   (for [x (range -1 2)
         y (range -1 2)]
     [x y])))
(def side-blues #{[-1 1], [-1 0], [-1 -1], [1 1], [1 0], [1 -1]})
(def middle-red #{[0 1], [0 0], [0 -1]})

; PROBLEM EXAMPLES
;
; This is a simple 3x3, all blues and no reds
;
;(hs/search-a-star hs/a-star-search (sonic/make-sonic-problem -2 3 
;      sonic/blue-3x3) {:position [-1 2], :red-balls #{}} 100)
;
; This is a 3x3 where the middle column is all red
;
; (hs/search-a-star hs/a-star-search (sonic/make-sonic-problem -2 3 
;      sonic/side-blue) {:position [-1 2], :red-balls sonic/middle-red} 100)

(defn make-sonic-problem
  "Create an instance of a problem of switching all blue-balls on a grid
   to red-balls"
  [min-range max-range blue-ball-set]
  {:goal? (partial origin-goal? blue-ball-set)
   :make-children (partial grid-children min-range max-range blue-ball-set)
   :heuristic (partial blue-left blue-ball-set)}
  )

