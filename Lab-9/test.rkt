(load "restaurant.scm")
(load "learning.scm")
(load "dtree.scm")

(define candidates
  (map car restaurant-attributes))

(define remove-val
  (lambda (val lst)
    (cond
      ((null? lst)
       '())
      ((equal? val (car lst))
       (remove-val val (cdr lst)))
      (else
       (cons (car lst)
             (remove-val val (cdr lst)))))))

(define second-candidate
  (remove-val "Pat" candidates))