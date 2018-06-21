(require "bp.scm")

;;
;; Procedure
;;   chain-probs
;;
;; Purpose
;;   Compute a list of CPTS for a chain-structured Bayesian network
;;
;; Parameters
;;   card, a positive integer
;;   len, a positive integer
;;
;; Produces
;;   probs, a list
;;
;; Preconditions
;;
;; Postconditions
;;   (= len (length probs))
;;   (= card (length (list-ref probs i)))
;;   Each entry in probs represents the CPT for a Bayesnet where each node has 
;;     one child except the last, and each node has one parent except the first.
;;
;; Practica
;;   The CPTS are not properly normalized.
;;
(define chain-probs
  (lambda (card len)
    (cons (map (lambda (num) (random)) (iota card)) ; Add first prior prob
	  (map (lambda (node)
		 (map (lambda (num)                  ; a list of ...
			(map (lambda (num) (random)) ; a list of 
			     (iota card)))           ; card random numbers
		      (iota card)))                  ; (having length card)
	       (iota len)))))
      
;;
;; Procedure
;;   chain-topology
;;
;; Purpose
;;   Construct the topology for chain-structured Bayesian network
;;
;; Parameters
;;   len, a positive integer
;;
;; Produces
;;   topology, a list
;;
;; Preconditions
;;
;; Postconditions
;;   (= len (length topology))
;;   (= (car (list-ref topology i)) (- i 1)) for all 1 <= i < len.
;;   (null? (list-ref topology 0)) since the first node has no parents
(define chain-topology
  (lambda (len)
    (cons null (map list (iota (- len 1))))))


;;
;; Procedure
;;   chain-sizes
;;
;; Purpose
;;   Construct the list of sizes (cardinality) for a chain network
;;
;; Parameters
;;   card, a positive integer
;;   len, a positive integer
;;
;; Produces
;;   sizes, a list
;;
;; Preconditions
;;
;; Postconditions
;;   (= len (length sizes))
;;   (= card (list-ref sizes i) ) for all 0 <= i < len.
(define chain-sizes 
  (lambda (card len)
    (make-list len card)))


;;
;; Procedure
;;   chain-network
;;
;; Purpose
;;   Construct a 
;;
;; Parameters
;;   card, a positive integer
;;   len, a positive integer
;;
;; Produces
;;   sizes, a list
;;
;; Preconditions
;;
;; Postconditions
;;   (= len (length sizes))
;;   (= card (list-ref sizes i) ) for all 0 <= i < len.
(define chain-network 
  (lambda (card len)
    (make-bayes-net
     (chain-sizes card len)
     (chain-topology len)
     (chain-probs card len))))