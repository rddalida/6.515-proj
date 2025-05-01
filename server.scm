;;; Server-side logic

(load "requests")
(load "tictactoe")

(define board (initialize-board))

;; (define s)

(define (make-server port-num)
  (let ((server-socket (open-tcp-server-socket port-num)))
    (pp 'server-socket-created)
    ;; (set! s server-socket)
    (let ((incoming-port (tcp-server-connection-accept
			  server-socket
			  #t ;; Block on Port
			  #f)))
      (pp 'connection-accepted)
      (let lp ((c-in (read-line incoming-port)))
	(pp (list 'received-from port-num c-in))
	(process-request-str c-in incoming-port)
	(lp (read-line incoming-port))))))

;; Server-side process request
(define (process-request-str c-in client-port)
  (let ((req (eval-str c-in)))
    (if (request? req)
	(process-request-server req client-port) ; using server version for now
	(error "Object was not a request"))))

(set! server-clients '())

(make-server 10001)
;; (close-tcp-server-socket s)
