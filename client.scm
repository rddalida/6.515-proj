;;; Client-side

(load "requests")
(load "tictactoe")

(define board (initialize-board))

(define (make-client port-num)
  (let ((socket (open-tcp-stream-socket "localhost" port-num)))
    socket))

(define port-num 10001)
(define c (make-client port-num))

(define id-num 0)

(define (initiate-request cont)
  (set! id-num (+ 1 id-num))
  (send-request c 'initial id-num cont)
  (process-request-str (read-line c) c))

;; handling of requests on the client side
(define (process-request-client req server)
  (let ((req-type (request-type req)))
    (cond
      ((eq? req-type 'initial)
       (error "client should not receive initial request"))
      ((eq? req-type 'prepare)
       (begin
	 (pp "Client received prepare, sending back to server")
	 (send-request server 'prepare id-num (request-body req))
	 ;; Now the client hangs until it receives the request back
	 ;; TODO check that the IDs are the same
	 (process-request-str (read-line server) server)
         ))
      ((eq? req-type 'commit)
       (begin
         (pp "Client received committed request, evaluating body")
	 (pp (list 'evaluated ((request-body req))))
         ))
      ((eq? req-type 'abort)
       (begin
         (pp "Client received aborted request")
         ))
      ((eq? req-type 'ignore)
       (begin
         (pp "Client received ignored request") ; print more stuff here
         ))
      (else
        (error "unknown request type")
       ))))

;; Client-side process request
(define (process-request-str c-in server-port)
  (let ((req (eval-str c-in)))
    (if (request? req)
	(process-request-client req server-port)
	(error "Object was not a request"))))

(initiate-request (lambda () (place-symbol "O" 1 2 board)))
(initiate-request (lambda () (print-board board)))
(initiate-request (lambda () (check-win board)))
