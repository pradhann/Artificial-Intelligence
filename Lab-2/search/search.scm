;; Lab: Uninformed Search
;; CSC 261
;;
;; File
;;   search.scm
;;
;; Authors
;;   Jerod Weinman 
;;     search 6Ps
;;     breadth-first-search 6Ps
;;     depth-first-search 6Ps and implementation 
;;     uniform-cost-search 6Ps and implementation
;;     depth-limited-search 6Ps
;;
;; Summary 
;;   Provides a collection of routines finding solutions to search space problem
;;
;; Provides
;;   (search start-state problem enqueue heuristic)
;;   (breadth-first-search start-state problem)
;;   (depth-first-search start-state problem)
;;   (uniform-cost-search start-state problem)
;;   (depth-limited-search start-state problem)
;;   (iterative-deepening-search start-state problem)

(load "node.scm")
(load "problem.scm")
(load "sort.scm")

;;
;; Procedure
;;   search
;;
;; Purpose
;;   Search a problem for a solution according to a specific enqueueing method
;;
;; Parameters
;;   start-state, a value
;;   problem, a problem
;;   enqueue, a procedure
;;   heuristic, a procedure
;;
;; Produces
;;   result, a list
;;
;; Preconditions
;;   enqueue takes a list of nodes to enqueue, a queue (list) of nodes, and 
;;      produces an updated queue of nodes
;;   heuristic takes a state (of problem) and produces a non-negative number
;;
;; Postconditions
;;   result is a list of the form (solution num-expansions), where
;;   solution is a list of actions that can be taken to reach a goal
;;   state from start-state or (list #f expansions) if no solution
;;   could be found, and num-expansions is a number indicating the
;;   number of times problem-expand-node is called.



(define search
  (lambda (start-state problem enqueue heuristic)
    ; Update the frontier and track the number of expansions 
    (let loop ((frontier 
                (list (node-init start-state heuristic))) 
               (expansions 0))
      (cond
        ;no elements in the frontier, return failure as (list #f expansions)
        ((null? frontier) (list #f expansions)) 
        (((problem-goal? problem) ; goal-state?
          (node-state (car frontier)))
         ;return the list of list of actions and the number of expansions
         ;  to reach goal state from start-node
         (list (node-extract-actions (car frontier)) expansions))
        (else
         ; return to loop by updating the frontier as per enqueue and
         ;  increasing the number of expansions by 1 
         (loop (enqueue (problem-expand-node problem (car frontier) heuristic)
                        (cdr frontier)) (add1 expansions)))))))

;;
;; Procedure
;;   breadth-first-search
;;
;; Purpose
;;   Find a solution to a problem using breadth-first search
;;
;; Parameters
;;   start-state, a value
;;   problem, a problem (list)
;;
;; Produces
;;   result, a list
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   result is a list of the form (solution num-expansions), where
;;   solution is a list of actions that can be taken to reach a goal
;;   state from start-state, or (list #f expansions) if no solution could
;;   be found
;;   (length solution) is minimal for start-state

(define breadth-first-search
  (lambda (start-state problem)
    (search
     start-state
     problem
     ;; Enqueueing procedure
     (lambda (new-nodes frontier) 
       (append frontier new-nodes))
     ;; Heuristic procedure -- always produces zero since DFS is uninformed
     (lambda (state) 0))))

;;
;; Procedure
;;   depth-first-search
;;
;; Purpose
;;   Find a solution to a problem using depth-first search
;;
;; Parameters
;;   start-state, a value
;;   problem, a problem
;;
;; Produces
;;   result, a list
;;
;; Preconditions
;;   
;;
;; Postconditions
;;   result is a list of the form (solution num-expansions), where
;;   solution is a list of actions that can be taken to reach a goal
;;   state from start-state, or (list #f expansions) if no solution could
;;   be found

(define depth-first-search
  (lambda (start-state problem)
    (search
     start-state
     problem
     ;; Enqueueing procedure
     (lambda (new-nodes frontier) 
       (append new-nodes frontier))
     ;; Heuristic procedure -- always produces zero since DFS is uninformed
     (lambda (state) 0))))
;;
;; Procedure
;;   depth-limited-search
;;
;; Purpose
;;   Find a solution to a problem using depth-limited search
;;
;; Parameters
;;   start-state, a value
;;   problem, a problem
;;   limit, an integer
;;
;; Produces
;;   result, a list
;;
;; Preconditions
;;   limit >= 0
;;
;; Postconditions
;;   result is a list of the form (solution num-expansions), where
;;   solution is a list of actions that can be taken to reach a goal
;;   state from start-state, or (list #f expansiosn) if no solution
;;   could be found (length solution) <= limit
;; Process
;;   depth-limited-search is similar to depth-first-search in how it enqueues
;;    its new-nodes. However, depth-limited-search imposes an extra conditional
;;    in its enqueue procedure where it prevents nodes deeper than limit to be
;;    added in the frontier 



(define depth-limited-search
  (lambda (start-state problem limit)
    (search
     start-state
     problem
     ;; Enqueueing procedure
     (lambda (new-nodes frontier)
       (let loop ([new-nodes new-nodes]
                  [frontier frontier])
         (cond
           [(null? new-nodes) frontier]
           ;test if the new-nodes are deeper than the limit, and only add
           ; nodes that are shallower than limit
           [(<= (node-depth (car new-nodes)) limit)
            (loop (cdr new-nodes) (cons (car new-nodes) frontier))]
           [else (loop (cdr new-nodes) frontier)])))
       
     ;; Heuristic procedure -- always produces zero since DFS is uninformed
     (lambda (state) 0))))

;;
;; Procedure
;;   uniform-cost-search
;;
;; Purpose
;;   Find a solution to a problem using uniform-cost search
;;
;; Parameters
;;   start-state, a value
;;   problem, a problem (list)
;;
;; Produces
;;   result, a list
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   result is a list of the form (solution num-expansions), where
;;   solution is a list of actions that can be taken to reach a goal
;;   state from start-state, or (list #f expansions) if no solution
;;   could be found
;;
;; Practica
;;   We could simply call list-key-insertion-sort for each of the new-nodes 
;;   given to the enqueue procedure. However, because all nodes in the queue
;;   will already be sorted, it is more efficient for us to do our own mini
;;   insertion sort on the node-path-cost.
(define uniform-cost-search
  (lambda (start-state problem)
    (search
     start-state
     problem
     ;; Enqueuing procedure
     (lambda (new-nodes sorted-queue)
       ;; Sort the new new nodes according to path cost
       (let ([sorted-children (list-keyed-insertion-sort
                               new-nodes
                               node-path-cost
                               <=)])
         ;; Insert the new nodes efficiently into the already-sorted frontier
         (let insert ([remaining sorted-children] ;; Items to insert
                      [queue sorted-queue])       ;; List of sorted nodes
           (cond
             [(null? remaining) ;; Nothing left to insert?
              queue] ;; Return the queue (which is sorted)
             [(null? queue) ;; If the queue is empty, we can simply return the
              remaining]      ;; remaining items, because they're already sorted
             [(< (node-path-cost (car remaining))  ;; Compare path costs
                 (node-path-cost (car queue)))  
              (cons (car remaining) ;; List of the first remaining item and the
                    (insert (cdr remaining) queue))] ;;rest inserted into queue
             [else
              (cons (car queue) ;; List of the queue front and insert remaining
                    (insert remaining (cdr queue)))])))) ;;  into rest of queue
     ;; Heuristic function
     (lambda (state) 0)))) ;; No heuristic function to speak of


;;
;; Procedure
;;   iterative-deepening-search
;;
;; Purpose
;;   Find a solution to a problem using iterative-deepening search
;;
;; Parameters
;;   start-state, a value
;;   problem, a problem (list)
;;
;; Produces
;;   result, a list
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   result is a list of the form (solution num-expansions), where
;;   solution is a list of actions that can be taken to reach a goal
;;   state from start-state, or the algorithm does not terminate if
;;   no soulution exists
;; Problem
;;   The algorithm is non-terminating for all start states that does not have
;;   a solution, after a certain limit has been reached, the algorithm repeats
;;   the same depth-limited seach ad infinitum
;; Process
;;   iteratice-deepening-search works by running
;;    (depth-limited-search start-state problem i) for i goes to 0 till
;;     infinity with a step increament of 1 for each iteration until
;;      it finds a solution.
;;   This implementaion of iterative-deepening-search never returns a failure
;;   even if it may be obvious that no solution exists.
;; Philosophy
;;   When the search algorithm does not find a solution, this implementation
;;   returns (list #f expansions) instead of just returning #f. The primary
;;   advantage of this decision is that we can track the number of
;;   node-expansions even if we have a failure, this is particularly helpful
;;   in finding the total number of expansions for an iterative-deepening-serach
;;   Moreover, the changing of #f to (list #f expansions) in the case of failure
;;   does not impact any other part of the code. 




(define iterative-deepening-search
  (lambda (start-state problem)
    (let loop ([counter 0]
               [expansions-so-far 0])
      (let ([result (depth-limited-search start-state problem counter)]) 
        (cond
          [(list? (car result)) (cons (car result)
                                (cons (+ expansions-so-far (cadr result)) '()))]
          [else
           (loop (add1 counter) (+ (cadr result) expansions-so-far))])))))










      
