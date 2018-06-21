;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Support routines for analyzing decision tree learning
;;
;; Preconditions
;;   decision-tree-learning is implemented and loaded
;;   decision-tree-accuracy is implemented and loaded
;;
;; Provides
;;   (run-trials examples attributes default num-trials)

(load "randperm.scm")  
(load "general.scm")


;;
;; Procedure
;;   rand-split-list
;;
;; Purpose
;;   Split a list's members randomly into two separate lists
;;
;; Parameters
;;   lst, a list
;;   
;; Produces
;;   result, a list
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   result is a list of two items
;;   (car result) and (cadr result) are both lists
;;   (append (car result) (cadr result)) is a permutation of lst

(define rand-split-list
  (lambda (lst)
    (riffle (permute-list lst))))


;;
;; Procedure
;;   rand-sub-list
;;
;; Purpose
;;   Extract a random sub-list of a specified length
;;
;; Parameters
;;   lst, a list
;;   len, a non-negative integer
;;   
;; Produces
;;   sub-list, a list
;;
;; Preconditions
;;   0 <= len < (length lst)
;;
;; Postconditions
;;   (length sub-list) = len
;;   All members of sub-list are members of list
;;   The probability of a member of lst appearing in sub-list is proportional 
;;     to its frequency in lst

(define rand-sub-list
  (lambda (lst len)
    (let ((lst-len (length lst)))
      (map (lambda (iter)
	     (list-ref lst (random lst-len)))
	   (iota len)))))


;;
;; Procedure
;;   run-trials
;;
;; Purpose
;;   Repeatedly apply decision-tree-learning to subsets of a train/test split
;;
;; Parameters
;;   examples, a list
;;   attribs, a list
;;   default, a value
;;   num-trials, a positive integer
;;
;; Produces
;;   accuracies, a list
;;
;; Preconditions
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;      whose and cdr is an association list. 
;;   Each association list has identical keys (and in the same order), 
;;      but potentially different values.
;;   attribs is an association list
;;   Each key in attribs is a key in the example association list
;;   Each value of an attribute in examples a member of the corresponding 
;;      association list values for attribs
;;   
;; Postconditions
;;   accuracies is a list of numbers between 0 and 1
;;   (length accuracies) = 21
;;   (list-ref accuracies i) is the test accuracy (averaged over num-trials runs) 
;;     of training a decision tree on i*5 examples

(define run-trials
  (lambda (examples attributes default num-trials)
    (let ((split (rand-split-list examples)))
      (map (lambda (len)
	     (/ (apply + (map (lambda (trial)
				(- 1 (decision-tree-accuracy
                                      (decision-tree-learning 
                                       (rand-sub-list (car split) len)
                                       attributes default)
                                      (cadr split))))
                              (iota num-trials)))
		num-trials))
	   (map (lambda (val) (* 5 val)) (iota 21))))))