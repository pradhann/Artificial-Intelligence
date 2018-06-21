;;
;; Procedure
;;   load-mushroom-examples
;;
;; Purpose
;;   Load mushroom example training data from a file
;;
;; Parameters
;;   examples-filename, a string
;;   attribs, a list
;;
;; Produces
;;   examples, a list
;;
;; Preconditions
;;   examples-filename points to a readable file
;;   attribs is an association list
;;   Each key in attribs is a key in the example association list
;;   Each value of an attribute in examples a member of the corresponding 
;;      association list values for attribs
;;
;; Postconditions
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;      whose and cdr is an association list. 
;;   Each association list has identical keys (and in the same order), 
;;      but potentially different values.
;;   
(define load-mushroom-examples
  (lambda (examples-filename attribs)
    (let ((port (open-input-file examples-filename)))
      (let example-loop ((label (read-char port)))
        (cond
         ((eof-object? label)
          ;; No more examples
          null)
         (else
          (read-char port) ; Read comma
          (cons (cons label
                      (let attrib-loop ((attribs (map car attribs))
                                        (attrib-val (read-char port)))
                        (cond
                         ((or (eof-object? attrib-val) 
                              (char=? attrib-val #\newline))
                          ;; No more attributes
                          null)
                         ((char=? attrib-val #\,)
                          ;; Comma separator, so continue
                          (attrib-loop (cdr attribs) (read-char port)))
                         (else
                          (cons (cons (car attribs) attrib-val)
                                (attrib-loop attribs
                                             (read-char port)))))))
                (example-loop (read-char port)))))))))


;;
;; Procedure
;;   load-mushroom-attributes
;;
;; Purpose
;;   Load the mushroom attribute list from a file
;;
;; Parameters
;;   attribs-filename, a string
;;
;; Produces
;;   
;;
;; Preconditions
;;   
;;
;; Postconditions
;;   

(define load-mushroom-attributes
  (lambda (attribs-filename)
    (let ((port (open-input-file attribs-filename)))
      (let attrib-loop ((attrib (read port)))
        (cond
         ((eof-object? attrib)
          null)
         (else
          (cons 
           (cons attrib
                 (let value-loop ((val (read port)))
                   (cond 
                    ((eof-object? val)
                     ;; No more values or attributes
                     null)
                    ((char=? (peek-char port) #\newline)
                     ;; No more values to read
                     (cons val null))
                    (else
                     (cons val (value-loop (read port)))))))
           (attrib-loop (read port)))))))))

