;; Lab: Sequential Probabilistic Inference
;; CSC 261 
;;
;; File
;;   analysis.scm
;;
;; Summary
;;   A collection of procedures and constants used to find the evidence
;;   of which typist typed a file without using the original file.
;;
;; Provides
;;   (path-original letter-num)
;;   (path-unknown letter-num)
;;   (path-typist-1 letter-num)
;;   (path-typist-2 letter-num)
;;   (train! counts path-fun letter-path)
;;   (evidence-calculator-individual letter-1 letter-2 path-test typed-by)
;;   (evidence-full-train path-test typed-by)
;;   (lang-trainer! lst lang-cond)
;;   (sensor-trainer! lst typo-cond path-fun)
;;   (restoration-general typist-1-letters typist-2-letters path-test
;;                        typed-by psuedo-obs)
;;   (restore-many lst init-vals)
;;   first-tests
;;   second-tests
;;   third-tests
;;   restore 

(load "charmodel.scm")
(load "hmm.scm")

;;; Procedure:
;;;   path-original
;;; Parameters:
;;;   letter-num, a string
;;; Purpose:
;;;   Returns the path to the original letter
;;; Produces:
;;;   letter-path, a string
;;; Preconditions:
;;;   There is a letter at mills/original corresponding to letter-num
;;; Postconditions:
;;;   letter-path is a path to a valid file
(define path-original
  (lambda (letter-num)
    (string-append
     (string-append "mills/original/" letter-num)
     ".txt")))

;;; Procedure:
;;;   path-unknown
;;; Parameters:
;;;   letter-num, a string
;;; Purpose:
;;;   Returns the path to the unknown letter
;;; Produces:
;;;   letter-path, a string
;;; Preconditions:
;;;   There is a letter at mills/corrupted/unknown corresponding to letter-num
;;; Postconditions:
;;;   letter-path is a path to a valid file
(define path-unknown
  (lambda (letter-num)
    (string-append
     (string-append "mills/corrupted/unknown/" letter-num)
     ".txt")))

;;; Procedure:
;;;   path-typist-1
;;; Parameters:
;;;   letter-num, a string
;;; Purpose:
;;;   Returns the path to the letter written by typist-1
;;; Produces:
;;;   letter-path, a string
;;; Preconditions:
;;;   There is a letter at mills/corrupted/typist1 corresponding to letter-num
;;; Postconditions:
;;;   letter-path is a path to a valid file
(define path-typist-1
  (lambda (letter-num)
    (string-append
     (string-append "mills/corrupted/typist1/" letter-num)
     ".txt")))

;;; Procedure:
;;;   path-typist-2
;;; Parameters:
;;;   letter-num, a string
;;; Purpose:
;;;   Returns the path to the letter written by typist-2
;;; Produces:
;;;   letter-path, a string
;;; Preconditions:
;;;   There is a letter at mills/corrupted/typist2 corresponding to letter-num
;;; Postconditions:
;;;   letter-path is a path to a valid file
(define path-typist-2
  (lambda (letter-num)
    (string-append
     (string-append "mills/corrupted/typist2/" letter-num)
     ".txt")))

;;; Procedure:
;;;   train!
;;; Parameters:
;;;   counts, a list of vectors
;;;   path-fun, a function used to compute the path to a letter
;;;   letter-path, a string representing a letter number
;;; Purpose:
;;;   To train counts on the letter corresponding to letter-path
;;; Produces:
;;;   Nothing, it is called for its side-effects
;;; Preconditions:
;;;   letter-path is a valid letter
;;;   path-fun returns the path for the correct typist
;;; Postconditions:
;;;   counts is trained on the letters given

(define train!
  (lambda (counts path-fun letter-path)
    (count-conditionals! (path-original letter-path)
                         (path-fun letter-path)
                         counts)))

;;; Procedure:
;;;   evidence-calculator-individual
;;; Parameters:
;;;   letter-1, a string representing a letter by typist-1 to train on
;;;   letter-2, a string representing a letter by typist-2 to train on
;;;   path-test, a string representing the letter to test against
;;;   typed-by, the integer 1 or 2 depending on who typed the letter in question
;;; Purpose:
;;;   To calculate the evidence of typist-1 having written the
;;;   letter represented by path-test using a sensor model and a language model
;;; Produces:
;;;   evidence, the evidence of typist-1 having written the letter
;;; Preconditions:
;;;   letter-1 is a valid letter written by typist-1
;;;   letter-2 is a valid letter written by typist-2
;;;   typed-by represents which typist wrote the letter represented by path-test
;;; Postconditions:
;;;   evidence is given in db, as defined by Jaynes' evidence
;;;   If evidence is positive, typist-1 is expected to have written the letter
;;;   If evidence is negative, typist-2 is expected to have written the letter

(define evidence-calculator-individual
  (lambda (letter-1 letter-2 path-test typed-by)
    (let*([counts-1
           (create-counts 28 1)]
          [counts-2
           (create-counts 28 1)]
          [lang-cond
           (create-counts 28 1)])

      ;Train the language model
      (count-transitions! (path-original letter-1) lang-cond)
      (count-transitions! (path-original letter-2) lang-cond)

      (let ([lang-marg
             (marginal-counts lang-cond)])

        ;Normalizing language model and language marginal
        (normalize-counts! lang-cond)
        (normalize-marginal-counts! lang-marg)
        
        ;Training the sensor model for typits 1 and typist 2
        (train! counts-1 path-typist-1 letter-1)
        (train! counts-2 path-typist-2 letter-2)
        ;Normalizing the sensor models 
        (normalize-counts! counts-1)
        (normalize-counts! counts-2)
      
        (let* (
               [test-letter
                (if (= typed-by 1)
                    (path-typist-1 path-test)
                    (path-typist-2 path-test))]
            
               [log-evidence-T1
                (log-evidence test-letter
                              counts-1
                              lang-cond
                              lang-marg)]

               [log-evidence-T2
                (log-evidence test-letter
                              counts-2
                              lang-cond
                              lang-marg)])
            
          (*
           10 (/ (- log-evidence-T1 log-evidence-T2)
                 (log 10))))))))


;;; Procedure:
;;;   evidence-full-train
;;; Parameters:
;;;   path-test, a string representing the letter to test against
;;;   typed-by, the integer 1 or 2 depending on who typed the letter in question
;;; Purpose:
;;;   To calculate the evidence of typist-1 having written the
;;;   letter represented by path-test using fully trained sensor model and
;;;   language model. The available letters are  1, 8, and 16 for typist-1 and
;;;   4, 9, and 18 for typist-2. The language model is trained using all 6
;;;   letters with their original files
;;; Produces:
;;;   evidence, the evidence of typist-1 having written letter path-test
;;; Preconditions:
;;;   typed-by represents which typist wrote the letter represented by path-test
;;; Postconditions:
;;;   evidence is given in db, as defined by Jaynes' evidence
;;;   If evidence is positive, typist-1 is expected to have written the letter
;;;   If evidence is negative, typist-2 is expected to have written the letter

(define evidence-full-train
  (lambda (path-test typed-by)
    (let*([counts-1
           (create-counts 28 1)]
          [counts-2
           (create-counts 28 1)]
          [lang-cond
           (create-counts 28 1)])

      ;Train the language model on all 6 available original letters 
      (count-transitions! (path-original "1") lang-cond)
      (count-transitions! (path-original "8") lang-cond)
      (count-transitions! (path-original "16") lang-cond)
      (count-transitions! (path-original "4") lang-cond)
      (count-transitions! (path-original "9") lang-cond)
      (count-transitions! (path-original "18") lang-cond)

      (let ([lang-marg
             (marginal-counts lang-cond)])

        ;Normalizing language model and language marginal
        (normalize-counts! lang-cond)
        (normalize-marginal-counts! lang-marg)
        
        ;Training the sensor model for typits 1 
        (train! counts-1 path-typist-1 "1")
        (train! counts-1 path-typist-1 "8")
        (train! counts-1 path-typist-1 "16")
        ;Training the sensor model for typits 2
        (train! counts-2 path-typist-2 "4")
        (train! counts-2 path-typist-2 "9")
        (train! counts-2 path-typist-2 "18")
        ;Normalizing the sensor models 
        (normalize-counts! counts-1)
        (normalize-counts! counts-2)
      
        (let* (
               [test-letter
                (cond
                  ((= typed-by 1) (path-typist-1 path-test))
                  ((= typed-by 2) (path-typist-2 path-test))
                  (else
                   (path-unknown path-test)))]
            
               [log-evidence-T1
                (log-evidence test-letter
                              counts-1
                              lang-cond
                              lang-marg)]

               [log-evidence-T2
                (log-evidence test-letter
                              counts-2
                              lang-cond
                              lang-marg)])
            
          (*
           10 (/ (- log-evidence-T1 log-evidence-T2)
                 (log 10))))))))



;;; Extra Credit : Restoration

;;; Procedure:
;;;   lang-trainer!
;;; Parameters:
;;;   lst, a list
;;;   lang-cond, a list of vectors 
;;; Purpose:
;;;   To train lang-cond on the letter corresponding to
;;;   each element from lst 
;;; Produces:
;;;   Nothing, it is called for its side-effects
;;; Preconditions:
;;;   all the elements of lst are valid lettter paths 
;;; Postconditions:
;;;   lang-cond is trained on the given list of letters using count-transitions!
(define lang-trainer!
  (lambda (lst lang-cond)
    (cond
      ((null? lst) lang-cond)
      (else
       (count-transitions! (path-original (car lst)) lang-cond)
       (lang-trainer! (cdr lst) lang-cond)))))

;;; Procedure:
;;;   sensor-trainer!
;;; Parameters:
;;;   lst, a list
;;;   typo-cond, a list of vectors
;;;   path-fun, a function used to compute the path to a letter
;;; Purpose:
;;;   To train typo-cond on the letter corresponding to
;;;   each element from lst 
;;; Produces:
;;;   Nothing, it is called for its side-effects
;;; Preconditions:
;;;   all the elements of lst are valid lettter paths from the same typist
;;;   path-fun returns the path for the correct typist of the correct typist 
;;; Postconditions:
;;;   typo-cond is trained on the given list of letters
;;;   using train! 

(define sensor-trainer!
  (lambda (lst typo-cond path-fun)
    (cond
      ((null? lst) typo-cond)
      (else
       (train! typo-cond path-fun  (car lst))
       (sensor-trainer! (cdr lst) typo-cond path-fun)))))



;;; Procedure:
;;;   restoration-general
;;; Parameters:
;;;   typist-1-letters, a list representing letters by typist-1 to train on
;;;   typist-2-letters, a list representing letters by typist-2 to train on
;;;   path-test, a string representing the letter to test against
;;;   typed-by, the integer 1 or 2 depending on who typed the letter in question
;;;   psuedo-obs, the number of psuedo-observations to initialize
;;;               the language model 
;;; Purpose:
;;;   To restore the letter indicated by path-test 
;;; Produces:
;;;  restored, a string of characters 
;;; Preconditions:
;;;   typist-1-letters contains valid letter written by typist-1
;;;   typist-2-letters contains valid letter written by typist-2
;;;   typed-by represents which typist wrote the letter represented by path-test
;;;   psuedo-obs is greater than 0 and an integer 
;;; Postconditions:
;;;   restored is the most likely sequence of characters of the original letter 
;;;   for the letter indicated by path-test, as per the dynamic
;;;   Bayes Net described by the trained sensor model and transistion model

(define restoration-general
  (lambda (typist-1-letters typist-2-letters path-test typed-by psuedo-obs)
    (let*([counts-1
           (create-counts 28 1)]
          [counts-2
           (create-counts 28 1)]
          [lang-cond
           (create-counts 28 psuedo-obs)])
      
      (lang-trainer! typist-1-letters lang-cond)
      (lang-trainer! typist-2-letters lang-cond)

      (let ([lang-marg
             (marginal-counts lang-cond)])

        ;Normalizing language model and language marginal
        (normalize-counts! lang-cond)
        (normalize-marginal-counts! lang-marg)
        
        ;Training the sensor model for typits 1 and typist 2
        (sensor-trainer! typist-1-letters counts-1 path-typist-1)
        (sensor-trainer! typist-2-letters counts-2 path-typist-2)
        ;Normalizing the sensor models 
        (normalize-counts! counts-1)
        (normalize-counts! counts-2)
        (let (
              [test-letter
               (cond
                 ((= typed-by 1) (path-typist-1 path-test))
                 ((= typed-by 2) (path-typist-2 path-test))
                 (else
                  (path-unknown path-test)))]
              [counts
               (cond
                 ((= typed-by 1) counts-1)
                 (else
                  counts-2))])
  
          (most-likely-sequence test-letter
                                counts lang-cond lang-marg))))))



;;; Procedure:
;;;   restore-many 
;;; Parameters:
;;;   lst, a list
;;;   init-vals, a list of integers 
;;; Purpose:
;;;   To restore the letter for multiple initialization of psuedo-observations
;;;   in the language model 
;;; Produces:
;;;  Nothing, called for side-effects  
;;; Preconditions:
;;;   all the numbers in init-vals are integers greater than 0 
;;;   lst contains valid set of parameters for typist-1-letters,
;;;   typist-2-letters path-test typed-by of the function restoration-general
;;;   Each element of init-vals is a valid input for psuedo-obs, the last
;;;   parameter for restoration-general. 
;;; Postconditions:
;;;   Called for side-effects 

(define restore-many
  (lambda (lst init-vals)
    (cond
      ((null? init-vals) null)
      (else
       (map (lambda (x)
              (display (caddr x))
              (display ", ")
              (display (count-errors-file (path-original (caddr x))
                                          (if (= (cadddr x)  1)
                                              (path-typist-1(caddr x))
                                              (path-typist-2 (caddr x)))))
                              
                
              (display ", ")
              (display (count-errors-list (path-original (caddr x))
                                          (restoration-general
                                           (car x)
                                           (cadr x)
                                           (caddr x)
                                           (cadddr x)
                                           (car init-vals))))
              (display ", ")
              (display (car init-vals))
              (display "\n"))
            lst)
       (restore-many lst (cdr init-vals))))))
        

;;; Declaration of various lists to pass as input for various tests 
(define first-tests
  '(("1" "4" "18" 2)
    ("1" "4" "9" 2)
    ("1" "4" "16" 1)
    ("1" "4" "8" 1)
    ("1" "9" "18" 2)
    ("1" "9" "4" 2)
    ("1" "9" "16" 1)
    ("1" "9" "8" 1)
    ("1" "18" "9" 2)
    ("1" "18" "4" 2)
    ("1" "18" "16" 1)
    ("1" "18" "8" 1)
    ("8" "4" "18" 2)
    ("8" "4" "9" 2)
    ("8" "4" "16" 1)
    ("8" "4" "1" 1)
    ("8" "9" "18" 2)
    ("8" "9" "4" 2)
    ("8" "9" "16" 1)
    ("8" "9" "1" 1)
    ("8" "18" "9" 2)
    ("8" "18" "4" 2)
    ("8" "18" "16" 1)
    ("8" "18" "1" 1)
    ("16" "4" "18" 2)
    ("16" "4" "9" 2)
    ("16" "4" "8" 1)
    ("16" "4" "1" 1)
    ("16" "9" "18" 2)
    ("16" "9" "4" 2)
    ("16" "9" "8" 1)
    ("16" "9" "1" 1)
    ("16" "18" "9" 2)
    ("16" "18" "4" 2)
    ("16" "18" "8" 1)
    ("16" "18" "1" 1)))

(define second-tests
  '(("2"  1)
    ("14" 1)
    ("17" 1)
    ("12" 2)
    ("13" 2)
    ("19" 2)))


(define third-tests
  '(("5" 0)
    ("6" 0)
    ("20" 0)
    ("21" 0)
    ("23" 0)
    ("24" 0)))

(define restore
  '(
    (("8" "16") ("4" "9" "18") "1" 1)
    (("1" "16") ("4" "9" "18") "8" 1)
    (("1" "8") ("4" "9" "18")  "16" 1)
    (("1" "8" "16") ( "9" "18") "4" 2)
    (("1" "8" "16") ("4"  "18") "9" 2)
    (("1" "8" "16") ("4" "9" ) "18" 2)))

;Tests to get the data, only called for side effects 
         
  

 ;Raw-results for single-letter attribution
(display "Single-letter Tests:\n")
(display "Format: (t1-letter t2-letter original written-by): evidence\n")
(display "---------------------------------------------------------\n")
(map (lambda (x)
       (display x)
       (display ": ")
       (display (evidence-calculator-individual (car x)
                                                (cadr x)
                                                (caddr x)
                                                (cadddr x)))
       (display "\n"))
     first-tests)
  
; Evidence for the 6 new attributed letters 
(display "\nEvidence for the 6 new attributed letters  \n")
(display "Format: (attributed-letter written-by): evidence\n")
(display "---------------------------------------------------------------\n")
(map (lambda (x)
       (display x)
       (display ": ")
       (display (evidence-full-train (car x)
                                     (cadr x)))

       (display "\n"))
     second-tests)

; Attribution for the 6 new unattributed letters, lacking the original 
(display "\nAttributing the 6 unattributed letters :\n")
(display "Format: letter-num : evidence\n")
(display "----------------------------------------------------\n")
(map (lambda (x)
       (display (car x))
       (display ": ")
       (display (evidence-full-train (car x)
                                     (cadr x)))
       (display "\n"))
     third-tests)

;Extra Credit Restoration
(display "\nErrors in the typed letters of which we have the original
 letter\n")
(display "Format: Letter-num, errors in corrupted, errors in restored,")
(display " number of psuedo-observations from 1 till 60 \n")
(display "script stops becuase of many files open error, so we commented out\n")
(display "---------------------------------------------------------\n")
;(display (restore-many restore (map (lambda(x) (+ 1  x)) (iota 60))))

;(display " number of psuedo-observations with a step of 10 \n")
;(display "---------------------------------------------------------\n")
;(display (restore-many restore (map (lambda(x) (* 10 x))
;                                    (map (lambda(x) (+ 1  x)) (iota 60)))))

;Restoration of the file and store the output in a text file 
;(list->file (restoration-general  '("8" "16" "1") '( "4" "9" "18") "19" 2 20)
;              "19.txt")
;(list->file (restoration-general  '("8" "16" "1") '( "4" "9" "18") "13" 2 20)
;            "13.txt")
;(list->file (restoration-general  '("8" "16" "1") '( "4" "9" "18") "12" 2 20)
;            "12.txt")
;(list->file (restoration-general  '("8" "16" "1") '( "4" "9" "18") "17" 1 20)
;            "17.txt")
;(list->file (restoration-general  '("8" "16" "1") '( "4" "9" "18") "14" 1 20)
;            "14.txt")
;(list->file (restoration-general  '("8" "16" "1") '( "4" "9" "18") "2" 1 20)
;            "2.txt")




