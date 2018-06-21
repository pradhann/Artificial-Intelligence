;; Lab: Heuristic Search
;; CSC 261
;;
;; File
;;   heuristic.scm
;;
;;
;; Summary 
;;   Provides a collection of heursitic functions for the jump problem 
;;
;; Provides
;;   (jump-heuristic jump-state)
;;   (extra-heuristic jump-state)


(require "jump.scm")


;;
;; Procedure
;;   jump-heuristic
;;
;; Purpose
;;   Estimate the cost (the number of steps needed to reach goal-state) for
;;   a given jump-state  
;;
;; Parameters
;;   jump-state, a list of integers that encodes the jump state for a
;;    jump-problem
;;   jump-state is (list N pos m), where N is the course length, pos is the
;;   agent's current position and m is the agent's momentum 
;;
;; Produces
;;   estimate, a non-negative integer
;;
;; Preconditions
;;   jump-state is a valid jump-state as per the constraints of the jump problem
;;
;; Postconditions
;;   estimate is a non-negative integer
;;   estimate is never less than (optmial-jump-speed jump-state)

(define jump-heuristic
  (lambda (jump-state)
    (let* ([optimal-jump-speed
            (lambda (jump-state)
              (let* ([constant
                      (* 2 ( - (car jump-state) (cadr jump-state)))]
                     [discriminant (- 1 (* -4 constant))])
                (ceiling (/ (sub1 (sqrt discriminant)) 2))))]
         
           [best-estimate (optimal-jump-speed jump-state)]
           [momentum (caddr jump-state)])
      (cond
        ((> momentum  (add1 best-estimate))
         +inf.0)
        ((< momentum  best-estimate)
         (add1 best-estimate))
        (else
         best-estimate)))))


;; Procedure
;;   extra-heuristic 
;;
;; Purpose
;;   Estimate the cost (the number of steps needed to reach goal-state) for
;;   a given jump-state  
;;
;; Parameters
;;   jump-state, a list of integers that encodes the jump state for a
;;    jump-problem
;;   jump-state is (list N pos m), where N is the course length, pos is the
;;   agent's current position and m is the agent's momentum 
;;
;; Produces
;;   estimate, a non-negative integer
;;
;; Preconditions
;;   jump-state is a valid jump-state as per the constraints of the jump problem
;;
;; Postconditions
;;   estimate is a non-negative integer
;;   estimate is never less than (optmial-jump-speed jump-state)

(define extra-heuristic
  (lambda (jump-state)
    (let* ([length (car jump-state)]
          [distance (- (car jump-state) (cadr jump-state))]
          [speed (caddr jump-state)])
      (cond
        ((= 0 distance) 0)
        ((< 2 (/ length distance))  speed)
        (else (- length speed))))))

