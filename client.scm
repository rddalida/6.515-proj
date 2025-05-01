;;; Client-side

(load "requests")
(load "tictactoe")

(define board (initialize-board))

(define port-num 10001)
(define c (make-client port-num))

(define id-num 1)

(define (initiate-request cont)
  (send-request c 'initial id-num cont)
  (set! id-num (+ 1 id-num))
  (process-request-str (read-line c) c))

;; Client-side process request
(define (process-request-str c-in server-port)
  (let ((req (eval-str c-in)))
    (if (request? req)
	(process-request-client req server-port)
	(error "Object was not a request"))))

(initiate-request (lambda () (place-symbol "O" 1 2 board)))
(initiate-request (lambda () (print-board board)))
(initiate-request (lambda () (check-win board)))
