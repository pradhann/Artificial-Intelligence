;;
;; File
;;   classify.scm
;;
;; Authors
;;   Jerod Weinman
;;     Documentation for decision-tree-classify
;;   YOUR NAME AND BOX# HERE
;;
;; Summary
;;   Implementation of decision-tree classification
;;
;; Provides
;;   (decision-tree-classify decision-tree instance)

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
