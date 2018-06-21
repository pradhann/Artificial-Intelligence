;;
;; File
;;   classify.scm
;;
;; Authors
;;   Jerod Weinman
;;     Documentation for decision-tree-classify
;;     Anonymous students
;;
;; Summary
;;   Implementation of decision-tree classification
;;
;; Provides
;;   (decision-tree-classify decision-tree     instance)


(load "learning.scm")
(load "dtree.scm")
(load "restaurant.scm")
(load "mushroom.scm")

(define mushroom-attribute
  (load-mushroom-attributes "mushroom-attribs.txt"))

(define mushroom-example
  (load-mushroom-examples "mushrooms.txt" mushroom-attribute))


(define mushroom-tree
  (decision-tree-learning
   mushroom-example mushroom-attribute #f))

;;
;; Procedure
;;   decision-tree-classify
;;
;; Purpose
;;   Classify an instance using a decision-tree
;;
;; Parameters
;;   decision-tree, a decision tree
;;   instance, an association list
;;
;; Produces
;;   label, a value
;;
;; Preconditions
;;   instance is an association list whose keys are attributes
;;   decision tree-is a decision tree, which is either a label or a list 
;;      whose car is an attribute and whose cdr is an association list 
;;      with attribute values as keys and decision trees as values.
;;
;; Postconditions
;;   label is a classification for the instance


(define decision-tree-classify
  (lambda (decision-tree classification)
    (cond
      ;base case, decision tree is only a boolean 
      ((not (pair? decision-tree)) decision-tree)
      (else
       (let* (
              ;root attribute of the decision tree
              [root-attr (car decision-tree)]
              [root-attr-val-pair
               (assoc root-attr classification)]
              [root-attr-val (cdr root-attr-val-pair)]
              [subtree-pair (assoc root-attr-val (cdr decision-tree))]
              ;subtree following the selected value for root attribute
              [next-dtree (cdr subtree-pair)])
         ;recursion on the selected subtree
         (decision-tree-classify next-dtree classification))))))
              
    
