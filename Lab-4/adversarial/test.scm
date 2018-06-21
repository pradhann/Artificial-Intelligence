(require "mancala.scm")
(require "mancala-player.scm")
(require "cutoff-minimax.scm")
(require "game.scm")
(require "minimax.scm")
(require "barranca.scm")
(require "evaluation1.scm")
(require "evaluation.scm")
(require "alphabeta.scm")



(define mancala (make-mancala-game))

; Takes the first action generated 
(define mancala-lazy-player
   (lambda (state)
      (caar ((game-successors-fun mancala) state))))


; Evaluation function for player 1
(define simple-player1-eval (simple-mancala-eval #t))
(define simple-player1 
  (make-cutoff-minimax-player mancala 4 simple-player1-eval))

;Evaluation function for player 2, 4 level deep search
(define simple-player2-eval (simple-mancala-eval #f))
(define simple-player2 
  (make-cutoff-minimax-player mancala 4 simple-player2-eval))





;;; Siyu
(define siyu-player1-eval
  (best-mancala-eval #t))
(define siyu-player1
  (make-cutoff-minimax-player mancala 4 siyu-player1-eval))

(define siyu-player2-eval
  (best-mancala-eval #f))
(define siyu-player2
  (make-cutoff-minimax-player mancala 4 siyu-player2-eval))

;;; Nripesh
(define nripesh-player1-eval
  (best-mancala-eval1 #t))
(define nripesh-player1
  (make-cutoff-minimax-player mancala 4 nripesh-player1-eval))

(define nripesh-player2-eval
  (best-mancala-eval1 #f))
(define nripesh-player2
  (make-cutoff-minimax-player mancala 4 nripesh-player2-eval))







(define barranca (make-barranca-game 4 8))
(define barranca-player1-utility (barranca-utility-fun #t 8))


