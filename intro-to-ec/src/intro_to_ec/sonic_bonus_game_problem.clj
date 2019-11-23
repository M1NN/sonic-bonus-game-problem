(ns intro-to-ec.sonic-bonus-game-problem)

(defn origin-goal?
  "A goal checking function that assumes the target
   position is the origin, i.e., [0 0]."
  [blue-ball-set]
  (empty? blue-ball-set))

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
  "Apply a move to a given position, yielding the new position"
  [position move]
  (vec (map + position move))) ;; TRYING TO FIGURE OUT IFS

(defn legal-coordinate
  "Limit our search to the space where the given coordinate
   is in the range [0, max-range)."
  [min-range max-range x]
  (and (>= x min-range) (< x max-range)))

(defn legal-state
  "Return true if both coordinates are legal and this position isn't
   in the 'wall' set."
  [min-range max-range red-ball-set position]
  (and (every? (partial legal-coordinate min-range max-range) position)
       (not (contains? red-ball-set position))))

(defn grid-children
  "Generate a list of all the possible child states
   reachable from the given initial position."
  [min-range max-range red-ball-set position]
  (filter (partial legal-state min-range max-range red-ball-set)
          (map (partial apply-move position) all-moves)))

(defn remove-blue
  [blue-ball-set position]
  (disj blue-ball-set position))

(defn add-red
  [red-ball-set position]
  (conj red-ball-set position))

(defn blue-left [blue-ball-set position]
  (if (contains? blue-ball-set position)
    (- (count blue-ball-set) 1) (count blue-ball-set)))

(def min-range -2)
(def max-range 3)
(def no-balls #{})
(def blue-3x3
  (set
   (for [x (range -1 2)
         y (range -1 2)]
     [x y])))

(defn make-sonic-problem
  "Create an instance of a simple problem of moving on a grid towards
   the origin. The ranges specify the bounds on the grid world, and the
   `wall-set` is a (possibly empty) set of positions that can't be entered
   or crossed."
  [min-range max-range red-ball-set blue-ball-set]
  {:goal? origin-goal?
   :make-children (partial grid-children min-range max-range red-ball-set)
   :heuristic (partial blue-left blue-ball-set)
   :remove (partial remove-blue)
   :add (partial add-red)
   :send-blue blue-ball-set
   :send-red red-ball-set}
  )
