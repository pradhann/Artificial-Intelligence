(require "mancala.scm")
(require "mancala-player.scm")
(require "cutoff-minimax.scm")
(require "game.scm")
(require "minimax.scm")
(require "barranca.scm")
(require "evaluation.scm")
(require "alphabeta.scm")
(require "reiley-val.scm")


(define barranca (make-barranca-game 4 8))
(define barranca-player1-utility (barranca-utility-fun #t 8))

(define mancala (make-mancala-game))
(define mancala-player1-eval (best-mancala-eval1 #t))

(define mancala-player1 
  (make-cutoff-minimax-player mancala 4 mancala-player1-eval))


(define mancala-lazy-player
   (lambda (state)
      (caar ((game-successors-fun mancala) state))))

(define mancala-player2-eval (best-mancala-eval- #f))

(define mancala-player2 
  (make-cutoff-minimax-player mancala 4 mancala-player2-eval))


(game-play mancala mancala-player1  mancala-player2)
;(alpha-beta-search barranca 
;                  (game-start-state barranca)
;                  5
;                  barranca-player1-utility)