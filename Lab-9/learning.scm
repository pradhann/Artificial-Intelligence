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
;;   (decision-tree-learning-helper examples attribs default candidates)
;;   (subtree example-range dtrees)


(load "restaurant.scm")
(load "dtree.scm")
(load "mushroom.scm")

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


(define choose-attribute
  (lambda (examples candidates attrib-values)
    (let ([examples examples]
          [attrib-values attrib-values])

      ;Recursively selecting the attribute with highest information-gain
      (let kernel ([candidates candidates]
                   [best-candidate null]
                   [best-information 0])
        (cond
          ((null? candidates)
           best-candidate)
          (else
           
           (let ([cur-information ;information gain from the current attribute
                  (information-gain examples (car candidates) attrib-values)])
             (cond
               ;if current attribute provides more information, replace the
               ;best-candidate with current attribute and best-infromation
               ;with the value of information from current attribute 
               ((> cur-information best-information)
                (kernel (cdr candidates) (car candidates) cur-information))
               (else
                ;recurse on the remaining list of attributes without any change
                ;to best-attribute or best-information 
                (kernel (cdr candidates)
                        best-candidate best-information))))))))))
              

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

(define decision-tree-learning
  (lambda (examples attribs default)
    (decision-tree-learning-helper
     examples attribs default (map car attribs))))
 



;;
;; Procedure
;;   decision-tree-learning-helper
;;
;; Purpose
;;   Learn a decision tree from data
;;
;; Parameters
;;   examples, a list
;;   attribs, a list
;;   default, a value
;;   candidates, list of remaining attributes that has not been
;;               selected for the decision tree
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
;;   The function is initially called with candidates = (map car attribs)
;;   All the elements of candidates are keys of attribs 
;;
;; Postconditions
;;   decision tree-is a decision tree, which is either a label or a list 
;;      whose car is an attribute and whose cdr is an association list 
;;      with attribute values as keys and decision trees as values.


(define decision-tree-learning-helper
  (lambda (examples attribs default candidates)
    (cond
      ;if examples is empty then return default value 
      ((null? examples) default)
      ;if all examples have the same classification then
      ;return the classification
      ((all-same-label? examples) (caar examples))
      ;if attributes is empty then return PLURALITY-VALUE(examples)
      ((null? candidates) (plurality-value examples))
      ;recursive case    
      (else
       (let*
           (;pick the attribute that provides highest information gain
            [next-attribute (choose-attribute examples candidates attribs)]
            ;possible values that next-attribute can take 
            [example-range (cdr (assoc next-attribute attribs))]
            ;recursively build subtrees for eacn value in example-range
            [dtrees
             (let subtree-builder
               ([example-range example-range])
               (cond
                 ((null? example-range) '())
                 (else
                  (cons
                   (decision-tree-learning-helper
                    ;filtered list of examples where next-attribute
                    ; takes a specific value 
                    (filter-examples-by-attribute-value
                     examples
                     next-attribute
                     (car example-range))
                    attribs
                    ;(plurality-value examples) is the new default 
                    (plurality-value examples)
                    ;remove next-attribute from unselected attributes 
                    (filter-list candidates next-attribute))
                   ;recursive call on the remaining values that 
                   ;next-atttribute can take 
                   (subtree-builder
                    (cdr example-range))))))])
         ;build the decision tree with next-attribute at the root 
         (cons next-attribute 
               (build-subtree example-range dtrees)))))))



;;; Procedure:
;;;   build-subtree
;;; Parameters:
;;;   example-range, a list of values 
;;;   dtrees, a list of decision trees 
;;; Purpose:
;;;   pair each value in example-range with its corresponding
;;;   decision tree in dtrees 
;;; Produces:
;;;   alist, list of pairs 
;;; Preconditions:
;;;   example-range and dtrees are of the same length
;;;   The ith value in example-range yields the ith decision tree in dtrees 
;;; Postcondition:
;;;   alist is a list of pairs
;;;   Let the attribute yielding example-range be next-attribute.
;;;   Then, (cons next-attribute aList) is a decision tree with
;;;   the root at next-attribute 



(define build-subtree
  (lambda (example-range dtrees)
    (cond
      ((null? example-range) '())
      (else
       (cons (cons (car example-range) (car dtrees))
             (build-subtree (cdr example-range) (cdr dtrees)))))))


    
    
