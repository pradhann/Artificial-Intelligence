;; Procedure:
;;  rand-perm
;; Purpose:
;;  Generate a random permutation of n elements
;; Parameters:
;;  n, a strictly positive integer
(define rand-perm
  (lambda (n)
    (perm-index 
     (rand-perm-helper '() n)
     (one-to-n-list n))))

;; Procedure:
;;  rand-perm-helper
;; Parameters:
;;  perm, a list of numbers representing the indices used so far
(define rand-perm-helper
  (lambda (perm n)
    (cond ((= n 0) '())
          ((= n 1) (cons 0 perm))
          (else (cons (random n) (rand-perm-helper perm (- n 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; one-to-n-list
;; Generate a list of numbers 1 to n
(define one-to-n-list
  (lambda (n)
    (let loop ([n n]
	       [lst '()])
      (if (zero? n)
	  lst
	  (loop (- n 1) (cons n lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; index-filter
;; Generate a new list without the element at a particular index
(define index-filter
  (lambda (filtered-index lst)
    (index-filter-helper 0 filtered-index lst)))

(define index-filter-helper
  (lambda (index filtered-index lst)
    (if (= index filtered-index)
        (cdr lst)
        (cons (car lst)
              (index-filter-helper (+ index 1) 
                                   filtered-index
                                   (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;perm-index
;; Unwrap the permutation generation by repeatedly filtering
;; out the index chosen from the subsequent possibilities
(define perm-index
  (lambda (perm-list index-list)
    (if (null? perm-list)
        '()
        (cons (list-ref index-list (car perm-list))
              (perm-index (cdr perm-list)
                          (index-filter (car perm-list)
                                        index-list))))))
; Split a list into two by taking alternating elements
(define riffle
  (lambda (lst)
    (if (null? lst)
        '()
        (list (riffle-inc lst)
              (riffle-inc (cdr lst))))))

;; Helper procedure for splitting a list into two by including the first element 
(define riffle-inc
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (car lst) (riffle-exc (cdr lst))))))
;; Helper procedure for splitting a list into two by excluding the first element 
(define riffle-exc
  (lambda (lst)
    (if (null? lst)
        '()
        (riffle-inc (cdr lst)))))

;; Inefficiently return a list permutation by using list-ref to index 
(define permute-list
  (lambda (lst)
    (map (lambda (idx) (list-ref lst (- idx 1))) (rand-perm (length lst)))))
