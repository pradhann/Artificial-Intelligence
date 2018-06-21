
;; Lab: Decision Tree Analysis 
;; CSC 261 
;;
;; File
;;   driver.scm
;;
;; Dependencies
;;  dtree.scm
;;  analysis.scm
;;  classify.scm
;;  mushroom.scm
;;  learning.scm

;; Summary
;;  Test for decision-tree-classify
;;
;; Provides
;;
;;



(load "learning.scm")
(load "dtree.scm")
(load "mushroom.scm")
(load "classify.scm")
(load "analysis.scm")

(require rackunit)
(require rackunit/text-ui)


;;; Define necessary helpers

(define mushroom-attribute
  (load-mushroom-attributes "mushroom-attribs.txt"))

(define mushroom-example
  (load-mushroom-examples "mushrooms.txt" mushroom-attribute))

(define restaurant-tree (decision-tree-learning restaurant-examples
                                                restaurant-attributes #t))

(define mushroom-tree
  (decision-tree-learning
   mushroom-example mushroom-attribute #f))



;; Test for  decision-tree-classify

(define test-decision-tree-classify
  (test-suite
   "Test of decision-tree-classify procedure"
   (test-case
    "Using restaurant-tree"
    (check-equal? (decision-tree-classify restaurant-tree
                                          (cdar restaurant-examples))
                  #t
                  "Did not solve the problem"))
   (test-case
    "Using mushroom-tree"
    (check-equal? (decision-tree-classify mushroom-tree
                                          (cdar mushroom-example))
                  #\p
                  "Did not solve the problem"))))

(display "Testing  test-decision-tree-function")
(newline)
(run-tests test-decision-tree-classify)
(newline)


;; Raw data for Problem 2
;; Part A
(display " Accuracy of the resulting decision tree on the same data for
 mushroom examples -------------------")
(newline)
(display "Accuracy: ")
(display (decision-tree-accuracy mushroom-tree mushroom-example))
(newline)
(newline)

;; Part B
(display "Data for Part B obtained by running run-trials")
(newline)
(display (run-trials mushroom-example mushroom-attribute #f 50))



