(load "chain.scm")
(load "chain-hmm.scm")
(require "bp.scm")

(define umbrella-probs
  (lambda (len transition sensor)
    (append (list (list 0.5 0.5)) ; Rain-0 - even odds
	    (make-list (- len 1) ; Rain 1 - Rain-N
		       (list transition
			     (map (lambda (p) (- 1 p)) transition)))
	    (make-list len
		       (list sensor
			     (map (lambda (p) (- 1 p)) sensor))))))
(define umbrella-network
  (lambda (len transition sensor)
    (make-bayes-net
     (chain-hmm-sizes 2 2 len)
     (chain-hmm-topology len)
     (umbrella-probs len transition sensor))))

(define (u d n)
  (cons (+ d n) 0))


(define umb-net (umbrella-network 30 (list 0.7 0.3) (list 0.9 0.2)))