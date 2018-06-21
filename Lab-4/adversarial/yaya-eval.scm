;; Lab: Title
;; CSC 261 
;;
;; File
;;   evaluation.scm
;;
;; Summary
;;   A collection of routines to assist in finding mancala-eval function
;;   Final eval function defined as best-mancala-eval
;;
;; Provides
;;   (best-mancala-eval player)
;;   (is-empty? state index)
;;   (is-reachable? state index player)
;;   (no-of-stones-in-mancala state player)
;;   (num-other-hole state player index)
;;   (total-no-of-stones state player)
;;   (pieces-stealable state player)

(module evaluation lang/plt-pretty-big
  (provide yaya-mancala-best-player1
           yaya-mancala-best-player2)
  
  
  (require "mancala.scm" "alphabeta.scm" "game.scm") ; add any other modules you may need

  ;;
  ;; Procedure
  ;;   best-mancala-eval
  ;;
  ;; Purpose
  ;;   Given a player, return a function that is used to determine
  ;;   the value of a given state for that player
  ;;
  ;; Parameters
  ;;   player, a boolean
  ;;
  ;; Produces
  ;;   fun, a function
  ;;
  ;; Preconditions
  ;; [none]
  ;;
  ;; Postconditions
  ;;   fun is a function that takes in a state and returns a real number
  (define best-mancala-eval
    (lambda (player)
      (lambda (state)
        (let ((board (cdr state)))
          (+ (* (no-of-stones-in-mancala state player) 5)
             (total-no-of-stones state player)
             (pieces-stealable state player))))))

  ;;
  ;; Procedure
  ;;   is-empty?
  ;;
  ;; Purpose
  ;;   Determine whether or not a given index of a state is empty
  ;;
  ;; Parameters
  ;;   state, a state
  ;;   index, an integer
  ;;
  ;; Produces
  ;;   result, a boolean
  ;;
  ;; Preconditions
  ;;   state is a valid state of mancala as defined in mancala.scm
  ;;   length(state) index >= 0
  ;;
  ;; Postconditions
  ;;   result is true/false
  (define is-empty?
    (lambda (state index)
      (if (zero? (list-ref state index))
          #t
          #f)))

  ;; Procedure
  ;;   is-reachable?
  ;;
  ;; Purpose
  ;;   Determine whether or not a index is reachable by the given
  ;;   player in a given state
  ;;
  ;; Parameters
  ;;   state, a state
  ;;   index, an integer
  ;;   player, a boolean
  ;; Produces
  ;;   result, a boolean
  ;;
  ;; Preconditions
  ;;   state is a valid state of mancala as defined in mancala.scm
  ;;   length(state) index >= 0
  ;;
  ;; Postconditions
  ;;   result is true/false

  (define is-reachable?
    (lambda (state index player)
      ;;cur is the starting index for both players. end is the last valid index
      ;;for the players
      (let loop ([cur (if player 1 8)]
                 [end (if player 7 14)])
        (cond [(>= cur end)
               #f]
              [(zero? (list-ref state cur))
               (loop (+ cur 1) end)]
              [else
               (if (= (+ (list-ref state cur) cur) index)
                   cur
                   (loop (+ cur 1) end))]))))
  ;; Procedure
  ;;   no-of-stones-in-mancala
  ;;
  ;; Purpose
  ;;   Determine the number of stones in a given player's mancala
  ;;   in a given state
  ;;
  ;; Parameters
  ;;   state, a state
  ;;   index, an integer
  ;;   player, a boolean
  ;; Produces
  ;;   result, an integer
  ;;
  ;; Preconditions
  ;;   state is a valid state of mancala as defined in mancala.scm
  ;;   length(state) index >= 0
  ;;
  ;; Postconditions
  ;;   result >= 0
  (define no-of-stones-in-mancala
    (lambda (state player)
      (if player (list-ref state 7) (list-ref state 14))))

  ;; Procedure
  ;;   num-other-hole
  ;;
  ;; Purpose
  ;;   Determine the number of stones in the hole opposite to the given index
  ;;
  ;; Parameters
  ;;   state, a state
  ;;   index, an integer
  ;;   player, a boolean
  ;; Produces
  ;;   result, an integer
  ;;
  ;; Preconditions
  ;;   state is a valid state of mancala as defined in mancala.scm
  ;;   length(state) index >= 0
  ;;
  ;; Postconditions
  ;;   result >= 0
  (define num-other-hole
    (lambda (state player index)
      (if player (list-ref state (+ index 7))
          (list-ref state (+ 1 (modulo (+ 7 index) 14))))))

  ;; Procedure
  ;;   total-no-of-stones
  ;;
  ;; Purpose
  ;;   Determine the sum of all of the stones in the given player's holes
  ;;   Not including the mancala
  ;;
  ;; Parameters
  ;;   state, a state
  ;;   player, a boolean
  ;; Produces
  ;;   result, an integer
  ;;
  ;; Preconditions
  ;;   state is a valid state of mancala as defined in mancala.scm
  ;;
  ;; Postconditions
  ;;   result >= 0  
  (define total-no-of-stones
    (lambda (state player)
      (let loop ([cur (if player 1 8)]
                 [end (if player 7 14)]
                 [sum-so-far 0])
        (cond [(>= cur end)
               sum-so-far]
              [else
               (loop (+ 1 cur) end (+ (list-ref state cur) sum-so-far))]))))

  ;; Procedure
  ;;   pieces-stealable
  ;;
  ;; Purpose
  ;;   Determine the number of stealable pieces from the opposite players' holes
  ;;
  ;; Parameters
  ;;   state, a state
  ;;   player, a boolean
  ;; Produces
  ;;   result, an integer
  ;;
  ;; Preconditions
  ;;   state is a valid state of mancala as defined in mancala.scm
  ;;
  ;; Postconditions
  ;;   result >= 0
  ;;   result += 1 for each hole
  ;;   result += 1 for each reachable hole
  ;;   result += x, where x is the num in the hole opposite to the
  ;;   reachable-empty hole
  ;;   
  (define pieces-stealable
    (lambda (state player)
      (let loop([cur (if player 1 8)]
                [end (if player 7 14)]
                [sum-so-far 0])
        (cond [(>= cur end)
               sum-so-far]
              [(is-empty? state cur)
               (if (is-reachable? state cur player)
                   (loop (+ 1 cur) end (+ sum-so-far
                                          (+ 2 (num-other-hole
                                                state player cur)))))
               (loop (+ 1 cur) end (+ 1 sum-so-far))]
              [else
               (loop (+ 1 cur) end sum-so-far)]))))

  (define mancala (make-mancala-game)) 

  (define yaya-mancala-best-player1
    (make-alpha-beta-player mancala 5 (best-mancala-eval #t)))

  (define yaya-mancala-best-player2
    (make-alpha-beta-player mancala 5 (best-mancala-eval #f)))
  ) ; module

;
;(require "game.scm")
;(require "minimax.scm")
;(require "barranca.scm")
;(require "tictactoe.scm")
;(require "mancala.scm")
;(require "mancala-player.scm")
;(require "cutoff-minimax.scm")
;
;
;  (define is-empty?
;    (lambda (state index)
;      (if (zero? (list-ref state index))
;          #t
;          #f)))
;  
;
;    (define test-state
;      (list #t 0 2 0 0 1 0 2 3 4 5 6 7 21 4))
;    ;;Checks if an index is reachable. If it is returns the first index from which it is
;    ;; else returns false
;          (define is-reachable?
;    (lambda (state index player)
;      ;;cur is the starting index for both players. end is the last valid index for the players
;      (let loop ([cur (if player 1 8)]
;                 [end (if player 7 14)])
;        (cond [(>= cur end)
;               #f]
;              [(zero? (list-ref state cur))
;              (loop (+ cur 1) end)]
;              [else
;               (if (= (+ (list-ref state cur) cur) index)
;                cur
;                (loop (+ cur 1) end))]))))
;         ;; 
;          (define no-of-stones-in-mancala
;            (lambda (state player)
;              (if player (list-ref state 7) (list-ref state 14))))
;          
;          (define num-other-hole
;            (lambda (state player index)
;              (if player (list-ref state (+ index 7))
;                  (list-ref state (+ 1 (modulo (+ 7 index) 14))))))
;          
;          (define total-no-of-stones
;            (lambda (state player)
;              (let loop ([cur (if player 1 8)]
;                         [end (if player 7 14)]
;                         [sum-so-far 0])
;                (cond [(>= cur end)
;                       sum-so-far]
;                      [else
;                       (loop (+ 1 cur) end (+ (list-ref state cur) sum-so-far))]))))
;          (define pieces-stealable
;            (lambda (state player)
;              (let loop([cur (if player 1 8)]
;                              [end (if player 7 14)]
;                              [sum-so-far 0])
;                      (cond [(>= cur end)
;                             sum-so-far]
;                            [(is-empty? state cur)
;                              (if (is-reachable? state cur player)
;                                  (loop (+ 1 cur) end (+ sum-so-far
;                                                         (+ 2 (num-other-hole
;                                                               state player cur)))))
;                              (loop (+ 1 cur) end (+ 1 sum-so-far))]
;                            [else
;                             (loop (+ 1 cur) end sum-so-far)]))))
;         ;;didnt work out     
;          (define best-mancala-eval
;            (lambda (player)
;              (lambda (state)
;               (let ((board (cdr state)))
;                 (+ (* (no-of-stones-in-mancala state player) 5)
;                    (total-no-of-stones state player)
;                    ;subtract they can steal from the num pieces we can steal
;                    (- (pieces-stealable state player) (/ (pieces-stealable state (not player))
;                                                          5)))))))
;          
;          (define best-mancala-eval-2
;            (lambda (player)
;              (lambda (state)
;               (let ((board (cdr state)))
;                 (+ (* (no-of-stones-in-mancala state player) 5)
;                    (total-no-of-stones state player)
;                    (pieces-stealable state player))))))
;          
;(define mancala (make-mancala-game))
;(define mancala-player1-eval (best-mancala-eval #t))
;(mancala-player1-eval (game-start-state mancala))
;(define mancala-player1 
;  (make-cutoff-minimax-player mancala 3 mancala-player1-eval))
;(define mancala-player2-eval (best-mancala-eval-2 #f))
;(define mancala-player2
;  (make-cutoff-minimax-player mancala 3 mancala-player2-eval))
;(game-play mancala mancala-player1 mancala-player2)

