;;;; Code for the tic-tac-toe player.
;;;; Abstracts out all the details, as an example of general use.
(load "client")
(load "tictactoe")

;; User interface
(define client-port)

(define (begin-sync port-num)
  (set! client-port (start-client port-num))
  'connected)

(define (run-in-sync thunk)
  (initiate-request client-port thunk))

(define board (initialize-board))

;; Testing
(begin-sync 28001)

;; client 1
(define frog-placed (run-in-sync (lambda () (place-symbol "🐸" 1 0 board))))
(display frog-placed)
;; client 2
(run-in-sync (lambda () (place-symbol "☯︎️" 2 3 board)))
(run-in-sync (lambda () (place-symbol "☯︎️" 2 4 board)))
(run-in-sync (lambda () (print-board board)))
;; client 1
(run-in-sync (lambda () (place-symbol "🐸" 1 1 board)))
(run-in-sync (lambda () (place-symbol "🐸" 1 2 board)))
(run-in-sync (lambda () (place-symbol "🐸" 1 2 board)))
(run-in-sync (lambda () (place-symbol "🐸" 2 3 board)))
(run-in-sync (lambda () (place-symbol "🐸" 1 3 board)))
(run-in-sync (lambda () (place-symbol "🐸" 1 4 board)))
(run-in-sync (lambda () (print-board board)))
(run-in-sync (lambda () (check-win board)))

(close-port client-port)
#|
|#
