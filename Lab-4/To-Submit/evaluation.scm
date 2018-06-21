
;; Lab: Adversarial Search 
;; CSC 261 
;;
;; File
;;   evaluation.scm
;;
;; Summary
;;   A collection of helper functions that help calculate a heuristic for 
;;   the mancala game
;; Provides
;;   (best-mancala-eval)


(module evaluation lang/plt-pretty-big
  (provide best-mancala-eval)
  
  (require "mancala.scm")
  (require "mancala-player.scm")
  (require "general.scm")
  (require "game.scm")
  (require "cutoff-minimax.scm")

        
  ;;; Procedure:
  ;;;   best-mancala-eval
  ;;; Parameters:
  ;;;   player, a boolean indicating whether it is the turn of MAX (#t)
  ;;;   or whether it is the turn of MIN (#f)
  ;;; Purpose:
  ;;;   Returns a function that takes a mancala state and returns the "utility"
  ;;;   for player being at that particular state
  ;;; Produces:
  ;;;   fun, a unary function
  ;;; Preconditions:
  ;;;   [No Additional]
  ;;; Postcondition:
  ;;;   The value returned by fun is proportional to the number
  ;;;   of stones in the player's
  ;;;   mancala, the total number of stones in the player's side of the board,
  ;;;   the "optimal" number of holes in the player's side and inversely
  ;;;   proportional to the maximum number of stones in in front of opponent's
  ;;;   empty hole. 
  ;;;   Three components of the board configuration is calculated.
  ;;;   1) mancala-val = stones in player's mancala
  ;;;   2) stones-val = difference of number of stones in the player's side of
  ;;;      the board and the opponent's side of the board
  ;;;   3) Let # of holes in the player's side be empty-holes
  ;;;      hole-val =  (if (< empty-hole 4) num (- 3 empth-hole))
  ;;;   4) hole-val-adj = hole with the largest number of stones directly 
  ;;;      infront of an oponent's empty hole
  ;;;      (+ (* 3 mancala-val) stones-val hole-val hole-val-adj))))) is 
  ;;;      returned by fun. This particular weighting scheme proved to be
  ;;;      the most potent after iterative skirmish between other weighting
  ;;;      schemes
  ;;; Process:
  ;;;   The rationale behind the process in this function is explained in 
  ;;;   greater detail in the essay accompanying Part 1 of Lab-4. 


  
  (define best-mancala-eval
    (lambda (player)
      (lambda (state)
        (let* ([mancala-val ((simple-mancala-eval player) state)]
               [one (first-half state)]
               [two (second-half state)]
               [stones-val  ((stones-diff player) one two)]
               [hole-list ((hole-diff player) one two)]
               [hole-val (car hole-list)]
               [hole-val-adj (flip-sign (cadr hole-list))])
          (+ (* 3 mancala-val) stones-val hole-val hole-val-adj)))))
          

 


  ;;; Procedure:
  ;;;   hole-index
  ;;; Parameters:
  ;;;   lst, a lst of numbers
  ;;; Purpose:
  ;;;  returns the list of indicies containing 0 stones
  ;;; Produces:
  ;;;   lst-zero, a list of indices
  ;;; Preconditions:
  ;;;    [No Additional]
  ;;; Postcondition:
  ;;;   For each element x that belongs to lst-zero, (list-ref lst x) == 0 and
  ;;;   (list-ref lst y) != 0 if y does not belong to lst-zero


  (define hole-index
    (lambda (lst)
      (let helper ([lst lst]
                   [index 0])
        (cond
          ((null? lst)
           '())
          ((zero? (car lst))
           (cons index
                 (helper (cdr lst) (add1 index))))
          (else
           (helper
            (cdr lst) (add1 index)))))))

  ;;; Procedure:
  ;;;   first-half
  ;;; Parameters:
  ;;;   state, list representing a state of the mancala game
  ;;; Purpose:
  ;;;   split state into the first half containing only small holes from MAX's
  ;;;   side of the board. Essentially indices 0 till 5 of board = (cdr state).
  ;;; Prodcues:
  ;;;   lst, lst of num
  ;;; Precondition:
  ;;;   state has to be a valid manacala game state
  ;;; Postconditions:
  ;;;  lst is the number of stones in MAX's side of the board in the
  ;;;  same order. Notice that the stones in MAX's mancala is not included.
  ;;;  The order is with repsect to the order of how the stones are moved.
  
  (define first-half
    (lambda (state)
      (let ((board (cdr state)))
        (let helper ([board board]
                     [index 0])
          (cond
            ((= index 6) '())
            (else
             (cons (car board) (helper
                                (cdr board) (add1 index)))))))))

  ;;; Procedure:
  ;;;   second-half
  ;;; Parameters:
  ;;;   state, list representing a state of the mancala game
  ;;; Purpose:
  ;;;   split state into the second half containing only small holes from MIN's
  ;;;   side of the board. Essentially indices 7 till 12 of board = (cdr state).
  ;;; Prodcues:
  ;;;   lst, lst of num
  ;;; Precondition:
  ;;;   state has to be a valid manacala game state
  ;;; Postconditions:
  ;;;  lst is the number of stones in MIN's side of the board in the
  ;;;  same order. Notice that the stones in MIN's mancala is not included.
  ;;;  The order is with repsect to the order of how the stones are moved 
  
  (define second-half
    (lambda (state)
      (let ((board (cdr state)))
        (let ([with-mancala ((nest cdr 7) board)])
          (reverse (cdr (reverse with-mancala)))))))

  

  
           
  ;;; Procedure:
  ;;;   stones-diff
  ;;; Parameters:
  ;;;   player, boolean indicating who's turn it is
  ;;; Purpose:
  ;;;  Produces a function that takes the first half and the second half of the
  ;;;  board and returns the difference of the stones in each side of the board
  ;;; Prodcues:
  ;;;  fun, a binry function
  ;;; Preconditions:
  ;;;  [No additional]
  ;;; Postcondition:
  ;;;  The two parameters taken by fun should be a list of numbers
  ;;;  When player is #t, fun returns num: the number of stones in MAX's
  ;;;  side of the board minus the number of stones in MIN's side of
  ;;;  the board. When player is #f, fun returns (flip-sign num)

  (define stones-diff
    (lambda (player)
      (lambda (one two)
        (let* ([diff (- (apply + one)
                        (apply + two))])
          (if player
              diff
              (flip-sign diff))))))

  ;;; Procedure:
  ;;;   hole-diff
  ;;; Parameters:
  ;;;   player, boolean indicating who's turn it is
  ;;; Purpose:
  ;;;  Produces a function that takes the first half and the second half of the
  ;;;  board and returns (list a b) where a,b are numbers
  ;;; Prodcues:
  ;;;  fun, a binry function
  ;;; Preconditions:
  ;;;  [No additional]
  ;;; Postcondition:
  ;;;  The two parameters taken by fun should be a list of numbers, fun prodcues
  ;;;  (list a b), where a is the utility based on the player's number of empty
  ;;;   hole and b is the maximun number of stones in the player's half such
  ;;;   that the hole infront has 0 stones. The reason for calculating this
  ;;;   value is to warn the player of potential captures that can be made by
  ;;;   oponnent. 
 
  
  (define hole-diff
    (lambda (player)
      (lambda (one two)
        (cond (player
               (let*
                   ;find indices where player-one has no stones
                   ([one-hole-index (hole-index one)]
                    ;find indices where player-two has no stones
                    [two-hole-index (hole-index two)]
                    ;number of empty holes for player-one
                    [one-hole-len (length one-hole-index)]
                    ;customized incentive for empty holes: having uptil 3 empty
                    ;holes is good, anything more than that is bad
                    [one-util (if (> one-hole-len 3)
                                  (- 3 one-hole-len)
                                  one-hole-len)]
                    ;list of number of stones in player's side such that the
                    ;adjacent hole for opponent is empty
                    [one-adj-list (map (lambda(num)
                                         (list-ref one (- 5 num)))
                                       two-hole-index)])
                 ;return a weighted list of the calculated values
                 (list (* 2 one-util) 
                       (maximal one-adj-list))))
              (else
               (let*
                   ;find indices where player-one has no stones
                   ([one-hole-index (hole-index one)]
                    ;find indices where player-two has no stones
                    [two-hole-index (hole-index two)]
                    ;number of empty holes for player-two
                    [two-hole-len (length two-hole-index)]
                    ;customized incentive for empty holes: having uptil 3 empty
                    ;holes is good, anything more than that is bad
                    [two-util (if (> two-hole-len 3)
                                  (- 3 two-hole-len)
                                  two-hole-len)]
                    ;list of number of stones in player's side such that the
                    ;adjacent hole for opponent is empty
                    [two-adj-list (map (lambda(num)
                                         (list-ref two (- 5 num)))
                                       one-hole-index)])
                 ;return a weighted list of the calculated values
                 (list (* 2 two-util) 
                       (maximal two-adj-list))))))))

        
  ;--------------------------------------------------------------------------
  ; GENERAL HELPER FUNCTIONS 
  ;--------------------------------------------------------------------------

  ;;; Acknowledgements: The nest procedure was obtained from Lab1 

  ;;; Procedure:
  ;;;   nest
  ;;; Parameters:
  ;;;  f, a unary procedure
  ;;;  n, a positive integer
  ;;; Purpose:
  ;;;    Return a function obtained from composing f with itself
  ;;;      n times
  ;;; Produces:
  ;;;   fun, a unary function
  ;;; Preconditions:
  ;;;   [No additional]
  ;;; Postcondition:
  ;;;   Let f^n(x) be the function obtained from composing f with itself
  ;;;    n times
  ;;;     fun(x) = f^n(x)

  

  (define nest
    (lambda (f n)
      (cond
        ((not (integer? n))
         (error "n must be a positive integer"))
        ((negative? n)
         (error "n cannot be a negative number"))
        ((zero? n)
         (error "n must be a positive integer"))
        (else
         (nest-helper f f n)))))

  ;;; Procedure:
  ;;;   nest-helper
  ;;; Parameters:
  ;;;  f, a unary procedure
  ;;;  f-cum, a unary procedure
  ;;;  n, a non-negative integer
  ;;; Purpose:
  ;;;    A helper procedure for nest that calls itself recursively until n is
  ;;;     zero; It passes f-cum as the cummulative function resuling
  ;;;      from composing f  
  ;;; Produces:
  ;;;   fun, a unary function
  ;;; Preconditions:
  ;;;   [No additional]
  ;;; Postcondition:
  ;;;   Let f^n(x) be the function obtained from composing f
  ;;;   with itself n times
  ;;;     fun(x) = f^n(x)


  (define nest-helper
    (lambda (f f-cum n)
      (cond
        ((zero? (sub1 n)) f-cum)
        (else
         (nest-helper f (lambda (x)
                          (f (f-cum x))) (sub1 n))))))

  ;;; Procedure:
  ;;;   maximal
  ;;; Parameters:
  ;;;  lst, a lst
  ;;; Purpose:
  ;;;   Return the maximal element of the list, if list is empty, return 0
  ;;; Produces:
  ;;;   num, a number
  ;;; Preconditions:
  ;;;   [No additional]
  ;;; Postcondition:
  ;;;   num is the larges number in lst
  ;;; Practica:
  ;;;   This function had to be created because (apply max '()) returns an error
 
  (define maximal
    (lambda (lst)
      (cond
        ((null? lst) 0)
        (else
         (apply max lst)))))

  
  ;;; Procedure:
  ;;;   flip-sign
  ;;; Parameters:
  ;;;  num, a number
  ;;; Purpose:
  ;;;   Flip the sign of num
  ;;; Produces:
  ;;;   negated, a number
  ;;; Preconditions:
  ;;;   [No additional]
  ;;; Postcondition:
  ;;;   negated = (- 0 num)


  (define flip-sign
    (lambda (num)
      (- 0 num)))



  ) ; module
