;;; Client-side

(load "requests")
(load "tictactoe")
(load "parallel")

(define board (initialize-board))

(define (make-client port-num)
  (let ((socket (open-tcp-stream-socket "localhost" port-num)))
    socket))

(define id-num 0)

(define (receive-request port)
  (process-request-str (read-line port) port))

(define (initiate-request port cont)
  (set! id-num (+ 1 id-num))
    (send-request port 'initial id-num cont))

;; handling of requests on the client side
(define (process-request-client req server)
  (let ((req-type (request-type req)))
    (cond
      ((eq? req-type 'initial)
       (error "client should not receive initial request"))
      ((eq? req-type 'prepare)
       (begin
	 ;; (pp "Client received prepare, sending back to server")
	 (send-request server 'prepare id-num (request-body req))
	 (receive-request server)
	 ;; Now the client hangs until it receives the request back
	 ;; TODO check that the IDs are the same
         ))
      ((eq? req-type 'commit)
       (begin
         ;; (pp "Client received committed request, evaluating body")
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

(define (start-client port-num)
  (let ((c (make-client port-num)))
        (create-thread #f
                       (lambda ()
                         (let lp ()
                           (if (char-ready? c)
                             (without-interrupts (lambda () (receive-request c)))
                             ())
                           (lp))))
	c))

;; Testing, run these in the given order on two clients after starting the server
#|
;; client 1
(define client-port (start-client 24001))
;; client 2
(define client-port (start-client 24002))

;; client 1
(initiate-request client-port (lambda () (place-symbol "X" 1 0 board)))
;; client 2
(initiate-request client-port (lambda () (place-symbol "O" 2 3 board)))
(initiate-request client-port (lambda () (place-symbol "O" 2 4 board)))
(initiate-request client-port (lambda () (print-board board)))
;; client 1
(initiate-request client-port (lambda () (place-symbol "X" 1 1 board)))
(initiate-request client-port (lambda () (place-symbol "X" 1 2 board)))
(initiate-request client-port (lambda () (place-symbol "X" 1 3 board)))
(initiate-request client-port (lambda () (place-symbol "X" 1 4 board)))
(initiate-request client-port (lambda () (print-board board)))
(initiate-request client-port (lambda () (check-win board)))

(close-port client-port)
|#
