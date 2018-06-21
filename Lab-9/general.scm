(define l-s
  (lambda (op left)
    (lambda (right)
      (op left right))))

(define r-s
  (lambda (op right)
    (lambda (left)
      (op left right))))

(define left-section l-s)
(define right-section r-s)

;;
;; Procedure
;;   iota
;;
;; Purpose
;;   Create a list of the first N integers
;;
;; Parameters
;;   n, an integer
;;
;; Produces
;;   lst, a list
;;
;; Preconditions
;;   n >= 0
;;
;; Postconditions
;;  (= index (list-ref lst index)) for 0 <= index < n
(define iota
  (lambda (n)
    (let loop ((list-so-far null)
               (i (- n 1)))
      (if (< i 0)
          list-so-far
          (loop (cons i list-so-far) (- i 1))))))

;;
;; Procedure
;;   index-of
;;
;; Purpose
;;   Find the index of a value in a list
;;
;; Parameters
;;   val, a value
;;   lst, a list
;;
;; Produces
;;   index, a value
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   if index is not #f, then (equal? (list-ref lst index) val) is #t
(define index-of
  (lambda (val lst)
    (let loop ((index 0)
	       (lst lst))
      (cond
       ((null? lst)
	#f)
       ((equal? (car lst) val)
	index)
       (else
	(loop (+ 1 index) (cdr lst)))))))

;;
;; Procedure
;;   filter-list
;;
;; Purpose
;;   Remove specific items from a list
;;
;; Parameters
;;   lst, a list
;;   val, a value
;;
;; Produces
;;   filtered, a list
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   (member? val filtered) = #f
;;   if (not (equal? (list-ref lst i) val)) then 
;;     (member? (list-ref lst i) filtered) with the same frequency as in lst
(define filter-list
  (lambda (lst val)
    (cond
     ((null? lst)
      null)
     ((equal? (car lst) val)
      (filter-list (cdr lst) val))
     (else
      (cons (car lst) (filter-list (cdr lst) val))))))

;;
;; Procedure
;;   filter-alist
;;
;; Purpose
;;   Remove specific keys (and their associated values) from an association list
;;
;; Parameters
;;   lst, a list
;;   val, a value
;;
;; Produces
;;   filtered, a list
;;
;; Preconditions
;;   lst is an association list
;;
;; Postconditions
;;   (member? val (map car filtered)) = #f
;;   if (not (equal? (car (list-ref lst i)) val)) then 
;;     (member? (car (list-ref lst i)) (map car filtered))
;;     with the same frequency as in lst
(define filter-alist
  (lambda (lst val)
    (cond
     ((null? lst)
      null)
     ((equal? (caar lst) val)
      (filter-alist (cdr lst) val))
     (else
      (cons (car lst) (filter-alist (cdr lst) val))))))
