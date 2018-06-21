;; Lab : Adversarial Search 
;; CSC 261 
;;
;; File
;;   driver.scm
;;
;; Summary
;;   Driver function that prints the expected value for the alpha-beta-search
;;   and runs alpha-beta-serach in a couple of barrance games. The purpoe of
;;   this is to check that pruning occurs as expected
;;
;; Provides
;;   Test for alpha-beta-search

;;;Required modules for testing alpha-beta-search in barranca games and testing
(require "mancala-player.scm")
(require "cutoff-minimax.scm")
(require "game.scm")
(require "minimax.scm")
(require "barranca.scm")
(require "evaluation.scm")
(require "alphabeta.scm")
(require rackunit)
(require rackunit/text-ui)

;;; Test 1
;;; We create a barrance game (make-barranca-game 3 3). MAX starts first and has
;;; 3 choices, {1,2,3}. We do not expect any pruning in the subtree when MAX
;;; choses 1. After evaluating this subtree, the value of alpha and v is
;;; updated to -1 for the root node. The recursive call then starts when MAX
;;; choses 2. In this call, when alpha-beta-search evalutes the subtree when
;;; MIN choses 1, it returns a value of -1. Since this values is less than or
;;; equal to alpha (-1), the recursive call is terminated. Similarly, for the
;;; final action that MAX can take by choosing 3, the recursive call is
;;; terminated after evaluating the first action from the MIN node. 

;Making the barrance game and player to run alpha-beta-search

(display "Test-1 for barranca game (make-barranca-game 3 3))\n")
(display "Expected Outcome:")
(display "As mentioned in the comments of driver.scm, we expect the following
paths to be explored:
a) (1 3) (2)
b) (1 2) (3)
After exploring these two paths, the root node will update it's v and alpha
to be -1. It now starts a new recursive call when MAX chooses 2.
c) (2 3) (1)
After this call, the value of MIN node doing the recursive call
updates v=-1. Since alpha is -1 as well, the path ((2 1) (3)) will be
pruned.
d) ((3 2) (1))
Similarly, the path ((3 1) (2)) will be pruned. MAX is losing in all the
cases, so it returns the first evaluated action, choosing 1, as the best
available action.\n\n")


(define barranca-game1 (make-barranca-game 3 3))
(define game1-MAX-utility (barranca-utility-fun #t 3))

(display "Result of running alpha-beta-search:\n")
(alpha-beta-search barranca-game1
                   (game-start-state barranca-game1)
                   5
                   game1-MAX-utility)


(display "\n\nTest-2 for barranca game (make-barranca-game 4 8))\n")
(display "Expected Outcome:")
(display "As mentioned in the comments of driver.scm, we expect the following
paths to be explored:
a) (3 1) (4 2)
b) (4 1) (3 2)
After exploring these two paths, the root node will update it's v and beta
to be -1. It now starts a new recursive call when Min chooses 3.
c) (2 1) (4 3)
After this call, the value of MAX node doing the recursive call
updates v=-1. Since beta is -1 as well, the path ((4 1) (2 3)) will be
pruned.
d) ((2 1) (3 4))
Similarly, the path ((3 1) (2 4)) will be pruned. Goes back to the first node,
we have :
e) ((3 2) (4 1))
f) ((4 2) (3 1))
g) ((1 2) (4 3))
h) ((4 2) (1 3))
i) ((1 2) (3 4))
j) ((3 2) (1 4))
This return a evaluation number of action 2 is 1, which update the alpha to 1.
Following prune is similar, for action 3 and action 4, we have:
k) ((2 3) (4 1))
l) ((4 3) (2 1))
m) ((2 4) (3 1))
n) ((3 4) (2 1))
While ((1 3) (4 2)), ((4 3) (1 2)), ((1 3) (2 4)), ((2 3) (1 4)), ((1 4) (3 2)),
((3 4) (1 2)), ((1 4) (2 3)), ((2 4) (1 3)) are all pruned.\n\n")

(define barranca-game1 (make-barranca-game 4 8))
(define game1-MAX-utility (barranca-utility-fun #t 8))

(display "Result of running alpha-beta-search:\n")
(alpha-beta-search barranca-game1
                   (game-start-state barranca-game1)
                   5
                   game1-MAX-utility)


(display "\n\n Test to compare alpha-beta-search and cutoff-minimax-search")

(define Test-of-comparison
  (test-suite
   "Tests of comparison"
   (test-case
    "barranca with depth 3"
    (check-equal? (alpha-beta-search barranca-game1
                   (game-start-state barranca-game1)
                   3
                   game1-MAX-utility)
                  (cutoff-minimax-search barranca-game1
                   (game-start-state barranca-game1)
                   3
                   game1-MAX-utility)))
    (test-case
    "barranca with depth 4"
    (check-equal? (alpha-beta-search barranca-game1
                   (game-start-state barranca-game1)
                   4
                   game1-MAX-utility)
                  (cutoff-minimax-search barranca-game1
                   (game-start-state barranca-game1)
                   4
                   game1-MAX-utility)))
    (test-case
    "barranca with depth 5"
    (check-equal? (alpha-beta-search barranca-game1
                   (game-start-state barranca-game1)
                   5
                   game1-MAX-utility)
                  (cutoff-minimax-search barranca-game1
                   (game-start-state barranca-game1)
                   5
                   game1-MAX-utility)))))

(run-tests Test-of-comparison)

                   




















