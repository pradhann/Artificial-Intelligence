;; Lab: Title
;; CSC 261 
;;
;; File
;;   driver.scm
;;
;; Dependencies
;;  search.scm
;;  jump.scm
;;  
;; Summary
;;   A collection of tests for the implementaion of search, bfs, dls and ids
;;
;; Provides
;;
;;


(load "search.scm")
(load "jump.scm")

(define course-length 25)
(define start (jump-start-state course-length))
(define dfs-sol (depth-first-search start (jump-problem course-length)))
(define bfs-sol (breadth-first-search start (jump-problem course-length)))
(define d1s-sol (depth-limited-search start (jump-problem course-length) 9))
(define ids-sol (iterative-deepening-search start (jump-problem course-length)))
(define ucs-sol (uniform-cost-search start (jump-problem course-length)))

(display (list 'bfs (length (car bfs-sol)) (cadr bfs-sol)))
(newline)
(display (list 'dfs (length (car dfs-sol)) (cadr dfs-sol)))
(newline)
(display (list 'ids (length (car ids-sol)) (cadr ids-sol)))
(newline)
(display (list 'ucs (length (car ucs-sol)) (cadr ucs-sol)))
