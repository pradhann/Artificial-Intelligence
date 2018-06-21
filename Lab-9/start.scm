(load "mushroom.scm") ; Load procedures for loading mushroom data
(load "dtree.scm")    ; Load decision-tree routines

; Load the attributes list for mushrooms
(define mushroom-attributes
  (load-mushroom-attributes "mushroom-attribs.txt"))
; Load all the mushroom examples (labels with attribute association lists)
(define mushroom-examples
  (load-mushroom-examples "mushrooms.txt" mushroom-attributes))
