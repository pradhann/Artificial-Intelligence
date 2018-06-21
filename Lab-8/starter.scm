(load "charmodel.scm")
(load "hmm.scm")
(load "lab-7.scm")

(define typist-1
  (create-counts 28 1))

(count-conditionals! (path-original "16")
                     (path-typist-1 "16")
                     typist-1)
(normalize-counts! typist-1)

(define typist-2
  (create-counts 28 1))
(count-conditionals! (path-original "4")
                     (path-typist-2 "4")
                     typist-2)
(normalize-counts! typist-2)




(define counts-structure
  (create-counts 28 1))

(count-transitions! (path-original "4") counts-structure)
(count-transitions! (path-original "16") counts-structure)

(define counts-marginal
   (marginal-counts counts-structure))


; Normalizing the counts-structure
(normalize-counts! counts-structure)


; Normalizing the counts-marginal
(normalize-marginal-counts! counts-marginal)

;(log-evidence (path-typist-1 "14") typist-1 counts-structure counts-marginal)
;(log-evidence (path-typist-1 "14") typist-2 counts-structure counts-marginal)



(count-errors-file (path-original "8") (path-typist-1 "8"))

(define restored-8-1
(most-likely-sequence (path-original "8")
                      typist-1 counts-structure counts-marginal))

(count-errors-list (path-original "8") restored-8-1)

(define restored-8-2
(most-likely-sequence (path-original "8")
                      typist-2 counts-structure counts-marginal))

(count-errors-list (path-original "8") restored-8-2)


