(require "bp.scm")
(load "chain.scm")
;;
;; Procedure
;;   chain-hmm-probs
;;
;; Purpose
;;   Compute a list of CPTS for a chain-structured Bayesian network
;;
;; Parameters
;;   card-state, a positive integer
;;   card-obs, a positive integer
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
;;   Each entry in probs represents the CPT for a Bayes net where each node has 
;;     one child except the last, and each node has one parent except the first.
;;
;; Practica
;;   The CPTS are not properly normalized.
;;
(define chain-hmm-probs
  (lambda (card-state card-obs len)
    (append (chain-probs card-state len) ; State CPTs
	    (map (lambda (node)
		   (map (lambda (obs) (random)) (iota card-obs)))
		 (iota len)))))
			  
      
;;
;; Procedure
;;   chain-hmm-topology
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
;;   (= (* 2 len) (length topology))
;;   (= (car (list-ref topology i)) (- i 1)) for all 1 <= i < len.
;;   (= i (car (list-ref toplogy (* 2 i))))  for all 1 <= i < len.
;;   (null? (list-ref topology 0)) since the first node has no parents
(define chain-hmm-topology
  (lambda (len)
    (append (chain-topology len) ; State nodes
	    (map list (iota len)))))        ; Observation nodes


;;
;; Procedure
;;   chain-sizes
;;
;; Purpose
;;   Construct the list of sizes (cardinality) for a chain network
;;
;; Parameters
;;   card-state, a positive integer
;;   card-obs, a positive integer
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
(define chain-hmm-sizes 
  (lambda (card-state card-obs len)
    (append (make-list len card-state)
	    (make-list len card-obs))))


;;
;; Procedure
;;   chain-hmm-network
;;
;; Purpose
;;   Construct a chain HMM network with random CPTs
;;
;; Parameters
;;   card-state, a positive integer
;;   card-obs, a positive integer
;;   len, a positive integer
;;
;; Produces
;;   net, a bayesian network
;;
;; Preconditions
;;
;; Postconditions
;;  
(define chain-hmm-network 
  (lambda (card-state card-obs len)
    (make-bayes-net
     (chain-hmm-sizes card-state card-obs len)
     (chain-hmm-topology len)
     (chain-hmm-probs card-state card-obs len))))