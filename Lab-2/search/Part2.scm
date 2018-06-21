


(load "search.scm")
(load "jump.scm")
(load "problem.scm")
(load "node.scm")
(load "8puzzle.scm")

;;; Making an easy 8-puzzle state


(define eight-puzzle (eight-puzzle-problem))
(define eight-puzzle-state-easy (random-eight-puzzle-state 4))
(display (eight-puzzle-state-board-list eight-puzzle-state-easy))

;;; Running various search on this problem state
(define bfs-easy1 (breadth-first-search eight-puzzle-state-easy
                                        eight-puzzle))

(define dfs-easy1 (depth-first-search eight-puzzle-state-easy
                                        eight-puzzle))

(define dls-easy1 (depth-limited-search eight-puzzle-state-easy
                                        eight-puzzle 4))

(define ucs-easy1 (uniform-cost-search eight-puzzle-state-easy
                                        eight-puzzle))

(define ids-easy1 (iterative-deepening-search eight-puzzle-state-easy
                                        eight-puzzle))

;;; function to show the solution for the various search algorithms on 8-puzzle
(define easy-sol-eight
  (lambda ()
    (list
    (list (length (car bfs-easy1)) (cadr bfs-easy1))
    (list (length (car dfs-easy1)) (cadr dfs-easy1))
    (list (length (car dls-easy1)) (cadr dls-easy1))
    (list (length (car ucs-easy1)) (cadr ucs-easy1))
    (list (length (car ids-easy1)) (cadr ids-easy1)))))

;;;  Making an easy jump problem

(define jump-puzzle-state-easy  (jump-start-state 5))
(define jump-problem-easy (jump-problem 5))


;;; Running various search on the easy jump problem
(define bfs-easy2 (breadth-first-search jump-puzzle-state-easy
                                        jump-problem-easy))

(define dfs-easy2 (depth-first-search jump-puzzle-state-easy
                                        jump-problem-easy))

(define dls-easy2 (depth-limited-search jump-puzzle-state-easy
                                        jump-problem-easy 5))

(define ucs-easy2 (uniform-cost-search jump-puzzle-state-easy
                                        jump-problem-easy))

(define ids-easy2 (iterative-deepening-search jump-puzzle-state-easy
                                        jump-problem-easy))

;;; function to show the solution for the various search algorithms on jump
(define easy-sol-jump
  (lambda ()
    (list
    (list (length (car bfs-easy2)) (cadr bfs-easy2))
    (list (length (car dfs-easy2)) (cadr dfs-easy2))
    (list (length (car dls-easy2)) (cadr dls-easy2))
    (list (length (car ucs-easy2)) (cadr ucs-easy2))
    (list (length (car ids-easy2)) (cadr ids-easy2)))))



;;; Making a difficult 8-puzzle state

(define eight-puzzle-state-hard (random-eight-puzzle-state 22))
(display (eight-puzzle-state-board-list eight-puzzle-state-hard))

;;; Running various search on this problem state
(define bfs-hard1 (breadth-first-search eight-puzzle-state-hard
                                        eight-puzzle))

(define dfs-hard1 (depth-first-search eight-puzzle-state-hard
                                        eight-puzzle))

(define dls-hard1 (depth-limited-search eight-puzzle-state-hard
                                        eight-puzzle 22))

(define ucs-hard1 (uniform-cost-search eight-puzzle-state-hard
                                        eight-puzzle))

(define ids-hard1 (iterative-deepening-search eight-puzzle-state-hard
                                        eight-puzzle))

;;; Function to give solution for 8problem hard for various search algorithms
(define hard-sol-eight
  (lambda ()
    (list
    (list (length (car bfs-hard1)) (cadr bfs-hard1))
    (list (length (car dfs-hard1)) (cadr dfs-hard1))
    (list (length (car dls-hard1)) (cadr dls-hard1))
    (list (length (car ucs-hard1)) (cadr ucs-hard1))
    (list (length (car ids-hard1)) (cadr ids-hard1)))))


;;;  Making a hard jump problem

(define jump-puzzle-state-hard  (jump-start-state 30))
(define jump-problem-hard (jump-problem 30))


;;; Running various search on the hard jump problem
(define bfs-hard2 (breadth-first-search jump-puzzle-state-hard
                                        jump-problem-hard))

(define dfs-hard2 (depth-first-search jump-puzzle-state-hard
                                        jump-problem-hard))

(define dls-hard2 (depth-limited-search jump-puzzle-state-hard
                                        jump-problem-hard 31))

(define ucs-hard2 (uniform-cost-search jump-puzzle-state-hard
                                        jump-problem-hard))

(define ids-hard2 (iterative-deepening-search jump-puzzle-state-hard
                                        jump-problem-hard))

;;; function to show the solution for the various search algorithms on jump
(define hard-sol-jump
  (lambda ()
    (list
    (list (length (car bfs-hard2)) (cadr bfs-hard2))
    (list (length (car dfs-hard2)) (cadr dfs-hard2))
    (list (length (car dls-hard2)) (cadr dls-hard2))
    (list (length (car ucs-hard2)) (cadr ucs-hard2))
    (list (length (car ids-hard2)) (cadr ids-hard2)))))


                      








 