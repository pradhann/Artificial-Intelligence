;;
;; File
;;   learning.scm
;;
;; Authors
;;   Jerod Weinman
;;     Documentation for decision-tree-learning and choose-attribute
;;   Anonymous students
;;
;; Summary
;;   Implementation of decision-tree learner
;;
;; Provides
;;   (decision-tree-learning examples attribs default)
;;   (choose-attribute examples candidates attrib-values)

;;
;; Procedure
;;   choose-attribute
;;
;; Purpose
;;   Find an optimal attribute to split on
;;
;; Parameters
;;   examples, an association list
;;   candidates, a list
;;   attrib-values, an association list
;;
;; Produces
;;   attrib, a value
;;
;; Preconditions
;;   candidates is non-empty
;;   attrib-values is non-empty
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;      whose and cdr is an association list. 
;;   Each association list has identical keys (and in the same order), 
;;      but potentially different values.
;;   Each member of candidates is a key in the association list.
;;   attrib-values is an association list
;;   Each value of the association list for an example's attribute is a member 
;;      of the values for attrib-values under the same key.
;;
;; Postconditions
;;   attrib is the member of candidates with the highest information gain 
;;   (entropy minus average conditional entropy)




;;
;; Procedure
;;   decision-tree-learning
;;
;; Purpose
;;   Learn a decision tree from data
;;
;; Parameters
;;   examples, a list
;;   attribs, a list
;;   default, a value
;;
;; Produces
;;   decision-tree, a decision-tree
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
;;
;; Postconditions
;;   decision tree-is a decision tree, which is either a label or a list 
;;      whose car is an attribute and whose cdr is an association list 
;;      with attribute values as keys and decision trees as values.



