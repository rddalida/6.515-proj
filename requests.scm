;;;; Procedures for server-client communcation:
;;;; make and receive requests of the form we're going to use.
;; You can load all definitions by using (load "requests.scm")

;; TODO: refactor code into server.scm, client.scm, and maybe common.scm?

(define x 100)

(define (set-x! nval)
  (set! x nval)
  nval)

(set-x! 3)

;; maintained by each client, incremented whenever they initiate a new request
;; needed so that client can tell which request was ignored
(define client-request-id 0)

(define (inc-client-request-id)
  (set! client-request-id (+ client-request-id 1))
  client-request-id)

(define (send-string str port)
  (write-string str port)
  (pp "sent!")
  (flush-output port))

;; Request framework
(define type-list
  (list 'initial 'commit 'prepare 'abort 'ignore))

(define (request-type req)
  (car req))

(define (request-id req)
  (cadr req))

(define (request-body req)
  (caddr req))

(define (request? req)
  (and (list? req)
       (= (length req) 3)
       (memq (request-type req) type-list)
       (number? (request-id req))
       (procedure? (request-body req))))

(request? (list 'initial 3 (lambda () (set-x! 3)))) ; true
(request? (list 'initial 3 (set-x! 3)))             ; false


;; handling of requests on the client side
(define (process-request-client req)
  (let ((req-type (request-type req)))
    (cond
      ((eq? req-type 'initial)
       (error "client should not receive initial request"))
      ((eq? req-type 'prepare)
       (begin
	 (pp "Client received prepare, sending back to server")
	 (send-request 8002 'prepare client-request-id (request-body req))
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

(define server-clients ()) ; list of all client ports, TODO: add logic for adding clients

(define num-clients (lambda () (length server-clients)))

(define prev-server-request 'commit) ; initialize to commit so first request isn't ignored

;; if the previous request wasn't commit or abort, then its still ongoing
;; ignore any new attempted initial requests until previous one finished
(define (ignore-request?)
  (if (or (eq? prev-server-request 'commit) (eq? prev-server-request 'abort))
      #f #t))

(define num-prepares-received 0)

(define (inc-num-prepares-received)
  (set! num-prepares-received (+ num-prepares-received 1))
  num-prepares-received)

(define (reset-prepares)
  (set! num-prepares-received 0)
  0)

;; sends request to every port in client-ports
(define (send-to-all-clients client-ports request)
  (for-each 
   (lambda (client)
     (send-request client (request-type request) (request-id request) (request-body request)))
   client-ports))

;; handling of requests on the server side
(define (process-request-server req client)
  ;; Code for adding clients to the server
  (if (memq client server-clients)
      ()
      (set! server-clients (cons client server-clients)))
  ;; TODO: what happens when clients disconnect?
  (let ((req-type (request-type req)))
    (cond 
     ((eq? req-type 'initial)
      (begin
	(pp "Server received an initial request")
	(if (ignore-request?)
	    (send-request client 'ignore (request-id request) (request-body request))
	    (send-to-all-clients server-clients (make-request 'prepare (request-id req) (request-body req))))
	))
      ((eq? req-type 'prepare)
       (begin
	 (pp "Server received a prepare request from a client")
	 (inc-num-prepares-received)
	 (if (= num-prepares-received (num-clients))
	     (begin
	       (send-to-all-clients server-clients (make-request 'commit (request-id req) (request-body req)))
	       (reset-prepares)))
         ))
      ((eq? req-type 'commit)
        (error "server should not receive commit request")
       )
      ((eq? req-type 'abort)
        (error "server should not receive abort request")
       )
      ((eq? req-type 'ignore)
        (error "server should not receive ignore request")
       )
      (else
       (error "unknown request type")
      )))) 

(define (make-request type id proc)
  (list type id proc))

(define my-req (make-request 'commit 3 (lambda () (set-x! 7))))

;;; Procedures for requests to strings and vice-versa

;; String methods
(define (eval-str input-str)
  (let ((parsed (read (open-input-string input-str))))
    ;; (pp "Received string to evaluate:\n")
    ;; (pp parsed)
    (eval parsed (nearest-repl/environment)))) ;; runs it on the current REPL

(define (parse-str input-str)
  (let ((parsed (read (open-input-string input-str))))
    parsed))

;; Turns a procedure into a parsable string
(define (procedure-to-str proc)
  (with-output-to-string
    (lambda () (pp proc))))

;; Turns a request into a string. When passed into eval-str,
;; the result is a request.
(define (request-to-str req)
  (with-output-to-string
    (lambda ()
      (display "(make-request '")
      (display (request-type req))
      (display " ")
      (display (request-id req))
      (display " ")
      (display (string-replace (procedure-to-str (request-body req))
			       #\newline
			       #\space))
      (display ")\n"))))

(request-to-str my-req)

(request? (eval-str (request-to-str my-req))) ; should be true

(define (process-request-str c-in client-port)
  (let ((req (eval-str c-in)))
    (if (request? req)
	(process-request-server req client-port) ; using server version for now
	(error "Object was not a request"))))

; (process-request-str (request-to-str (make-request 'commit 3 (lambda () 3)))) ; needs client port

;; Allows to "stall" - used from https://github.com/lrsjohnson/scheme-mapreduce
(define (make-server port-num)
  (let ((server-socket (open-tcp-server-socket port-num)))
    (pp 'server-socket-created)
    (let ((incoming-port (tcp-server-connection-accept
			  server-socket
			  #t ;; Block on Port
			  #f)))
      (pp 'connection-accepted)
      (let lp ((c-in (read-line incoming-port)))
	(pp (list 'received-from port-num c-in))
	(process-request-str c-in incoming-port)
	;; logic based on c-in
	(lp (read-line incoming-port))))))

(define (make-client port-num)
  (let ((socket (open-tcp-stream-socket "localhost" port-num)))
    socket))

(define (send-client-request port type id cont)
  (send-request port type id cont)
  (inc-client-request-id))

;; Sends a request to the given i/o port.
;; port - output port to send requests to
;; type - one of 'initial, 'prepare, 'commit, 'abort, 'ignore
;; id   - an integer
;; cont - continuation. a procedure called by the server when
;;        the request is determined to have passed
(define (send-request port type id cont)
  (send-string (request-to-str (make-request type id cont))
	       port)
  ;; TODO return either 'request-ignored if the request was ignored, or
  ;; the return value of cont when it was run on the server.
  ;; Needs some async-await things, probably
  )

;;; How to send and receive requests

#|
;; Run on the server...
(make-server 8002)

;; Run on the client...
(define c (make-client 8002))
(send-request c 'commit 1 (lambda () (+ 2000 25)))

;; The server should receive the request...
;(received-from 8002 "(make-request 'commit 1 (lambda () (+ 2000 25)))")
;(evaluated 2025)
|#
