;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Support routines for learning decision trees
;;
;; Provides
;;   (decision-tree-accuracy decision-tree examples)
;;   (all-same-label examples)
;;   (plurality-value examples)
;;   (filter-examples-by-attribute-value examples attribute value)
;;   (information examples)
;;   (information-gain examples attrib attrib-values)
;;   (label-counts examples)

(load "general.scm")


;;
;; Procedure
;;   decision-tree-accuracy
;;
;; Purpose
;;   Measure accuracy of decision-tree classifier on several examples
;;
;; Parameters
;;   decision-tree, a decision tree
;;   examples, a list
;;
;; Produces
;;   accuracy, a number
;;
;; Preconditions
;;   Procedure decision-tree-classify is defined
;;   decision-tree is a decision tree, which is either a label or a list 
;;      whose car is an attribute and whose cdr is an association list 
;;      with attribute values as keys and decision trees as values.
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;      whose and cdr is an association list. 
;;   Each association list has identical keys (and in the same order), 
;;      but potentially different values.
;;   decision-tree contains query keys that are in every instance in examples
;;
;; Postconditions
;;   0 <= accuracy <= 1 is the fraction of correctly classified examples, that is
;;      the number of times 
;;        (equal? (decision-tree-classify decision-tree 
;;                                        (cdr (list-ref examples i)))
;;                (car (list-ref examples i)))
;;      for 0 <= i < (length examples), divided by (length examples).
(define decision-tree-accuracy
  (lambda (decision-tree examples)
    (/ (apply + (map (lambda (example)
		       (if (equal? (car example) 
				   (decision-tree-classify decision-tree 
							   (cdr example)))
			   1
			   0))
		     examples))
       (length examples))))


;;
;; Procedure
;;   all-same-label?
;;
;; Purpose
;;   Determine whether a list of examples all have the same classification
;;
;; Parameters
;;   examples, a list
;;
;; Produces
;;   same, a boolean
;;
;; Preconditions
;;   examples is a non-empty list
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;
;; Postconditions
;;   
(define all-same-label?
  (lambda (examples)
    (let ((target (caar examples)))
      (let loop ((remaining (cdr examples)))
	(or (null? remaining)
	    (and (equal? (caar remaining) target)
		 (loop (cdr remaining))))))))

;;
;; Procedure
;;   plurality-value
;;
;; Purpose
;;   Returns the class label with the highest frequency
;;
;; Parameters
;;   examples, a list
;;
;; Produces
;;   label, a value
;;
;; Preconditions
;;   examples is a non-empty list
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;
;; Postconditions
;;   label occurs more often than any other value in (map car examples)
(define plurality-value
  (lambda (examples)
    (let ((counts (label-counts examples)))
      ;; Find highest count value
      (let max-loop ((argmax (caar counts)) ; Initial best is the first label
                     (maxval (cdar counts)) ; Initial max is  first count
                     (counts (cdr counts))) ; Remaining counts
        (cond
         ((null? counts) ; No more counts to check
          argmax)        ; return maximizing label
         ((> (cdar counts) maxval) ; If leading count is greater than max so far
	  ; Continue looping with updated label and max value
          (max-loop (caar counts) (cdar counts) (cdr counts)))
         (else ; Otherwise
	  ; Continue looping with the same label and max value
          (max-loop argmax maxval (cdr counts))))))))


;;
;; Procedure
;;   filter-examples-by-attribute-value
;;
;; Purpose
;;   Retrieve examples with a particular attribute value
;;
;; Parameters
;;   examples, a list
;;   attribute, a value
;;   value, a value
;;
;; Produces
;;   filtered-examples, a list
;;
;; Preconditions
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;      whose and cdr is an association list. 
;;   Each association list has identical keys (and in the same order), 
;;      but potentially different values.
;;   attribute is a key in the association list.
;;
;; Postconditions
;;   filtered-examples contains all (and only) entries from examples that have 
;;     (attribute value) as a member of their association list.
(define filter-examples-by-attribute-value
  (lambda (examples attribute value)
    (let filter ((examples examples))
      (cond
       ((null? examples)
        ;; No more examples.
        null)
       ((equal? (cdr (assoc attribute (cdar examples))) value)
        ;; Matching example. Include it
        (cons (car examples)
              (filter (cdr examples))))
       (else
        ;; Not a matching example. Don't include it.
        (filter (cdr examples)))))))


;;
;; Procedure
;;   information
;;
;; Purpose
;;   Compute the information (Shannon entropy in bits) in the example class labels
;;
;; Parameters
;;   examples, a list
;;
;; Produces
;;   info, a number
;;
;; Preconditions
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;
;; Postconditions
;;   info >= 0
(define information
  (lambda (examples)
    (if (null? examples) ; No examples has no information/uncertainty
        0
        (let* ((counts (map cdr (label-counts examples))) ; counts of all labels
               (total (apply + counts)))                  ; = (length examples)
          (/ (- (log total) ; log of the total number minus
		; dot product of counts/total and log(counts) ~ E[log(label)]
                (/ (apply + (map * counts (map log counts))) total))
             (log 2)))))) ; divide by log 2 to convert units from nats to bits

;;
;; Procedure
;;   information-gain
;;
;; Purpose
;;   Calculate the information gain (measured in bits) for an attribute
;;
;; Parameters
;;   examples, a list
;;   attrib, a value
;;   attrib-values, an association list
;;
;; Produces
;;   gain, a number
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
;;   attrib is a key in attribs (an by extension, in the association list 
;;      for an example)
;;
;; Postconditions
;;   gain >= 0

(define information-gain
  (lambda (examples attrib attrib-values)
    (let* ((info (information examples)) 
                                        ; Information in example labels
           (values (cdr (assoc attrib attrib-values)))
                                        ; All values for attribute of interest
           (remainder
                                        ; Average conditional information
                                        ; given attribute
            (/
             (apply 
              + ;; Sum over each attribute value
              (map 
               (lambda (attrib-value)
                 (let ((sub-examples 
                                        ; Examples with a given attribute value
                        (filter-examples-by-attribute-value 
                         examples
                         attrib
                         attrib-value)))
                                        ; Number of sub-examples * information  
                                        ; Divided by total number of examples
                   (* (length sub-examples) (information sub-examples))))
                                        ; Calculate conditional information
                                        ; for each attribute value
               values))
             (length examples))))
      (- info remainder))))

;;
;; Procedure
;;   label-counts
;;
;; Purpose
;;   Build an association list of counts for each class label
;;
;; Parameters
;;   examples, a list
;;
;; Produces
;;   counts, a list
;;
;; Preconditions
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;
;; Postconditions
;;   Each key in the association list counts is the car of an entry in examples
;;   cdr of each entry in counts is the number of times the key appears as a 
;;      label in examples.
;;
;;  Practica
;;    Due to the "constraints" of pure functional programming, this implementation
;;    is O(NL) where N is the number of examples and L is the number of labels.
(define label-counts
  (let ((increment-count 
         (lambda (counts label)
           (let loop ((counts counts))
             (cond
              ((null? counts)
               ;; Didn't find the label. Add it
               (list (cons label 1)))
              ((equal? label (caar counts))
               ;; Found the label. Increment the cdr of the alist entry.
               (cons 
                (cons (caar counts)
                      (+ 1 (cdar counts)))
                (cdr counts)))
              (else
               ;; Not a match. Go to next count.
               (cons (car counts)
                     (loop (cdr counts)))))))))
    (lambda (examples)
      ;; Build an association list of the label counts
      (let counts-loop ((counts null)
                        (examples examples))
        (if (null? examples)
            counts
            (counts-loop (increment-count counts
                                          (caar examples))
                         (cdr examples)))))))
