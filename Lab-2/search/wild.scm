(load "search.scm")
(load "jump.scm")
(load "problem.scm")
(load "node.scm")
(load "8puzzle.scm")

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