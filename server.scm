;;; Server-side logic

(load "requests")
(load "tictactoe")
(load "parallel")

(define board (initialize-board))

(define s 0)

(define request-history '()) ; stores history of all committed requests

;; Allows to "stall" - used from https://github.com/lrsjohnson/scheme-mapreduce
(define (make-server port-num)
  (let ((server-socket (open-tcp-server-socket port-num)))
    (pp 'server-socket-created)
    (set! s server-socket)
    (let ns ((incoming-port (tcp-server-connection-accept
			  server-socket
			  #t ;; Block on Port
			  #f)))
      (pp 'connection-accepted)
      (add-client incoming-port)
      (let lp ((c-in (read-line incoming-port)))
        (pp (list 'received-from port-num c-in))
        (if (eof-object? c-in)
          (begin
            (disconnect-client incoming-port)
            (ns (tcp-server-connection-accept server-socket #t #f)))
          (begin
            (process-request-str c-in incoming-port)
            (lp (read-line incoming-port))))))))

(define server-clients ()) ; list of all client ports

(define num-clients (lambda () (length server-clients)))

(define prev-server-request 'commit) ; initialize to commit so first request isn't ignored

;; if the previous request wasn't commit or abort, then its still ongoing
;; ignore any new attempted initial requests until previous one finished
(define (ignore-request?)
  (and (not (eq? prev-server-request 'commit)) (not (eq? prev-server-request 'abort))))

;; tracking prepares received by clients
(define num-prepares-received 0)

(define (inc-num-prepares-received)
  (set! num-prepares-received (+ num-prepares-received 1))
  num-prepares-received)

(define (reset-prepares)
  (set! num-prepares-received 0)
  0)

(define (add-client incoming-port)
      (set! server-clients (cons incoming-port server-clients))
      (for-each (lambda (req) ; send each committed request to new client
                  (send-request incoming-port (request-type req) (request-id req) (request-body req)))
                request-history))

(define (disconnect-client client-port)
  (set! server-clients (delete client-port server-clients))
  (close-port client-port))

;; sends request to every port in client-ports
(define (send-to-all-clients client-ports request)
  (for-each 
   (lambda (client)
     (send-request client (request-type request) (request-id request) (request-body request)))
   client-ports))

;; handling of requests on the server side
(define (process-request-server req client)
  ;; Code for adding clients to the server
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
	 (pp num-prepares-received)
	 (pp (num-clients))
	 (if (>= num-prepares-received (num-clients))
	     (begin
	       (send-to-all-clients server-clients (make-request 'commit (request-id req) (request-body req)))
               (set! request-history (cons (make-request 'commit (request-id req) (request-body req)) request-history)) ; add req to request history
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

;; Server-side process request
(define (process-request-str c-in client-port)
  (let ((req (eval-str c-in)))
    (if (request? req)
	(process-request-server req client-port)
	(error "Object was not a request"))))

;; How to support multiple clients
(set! server-clients '())

(parallel-execute
 (lambda () (make-server 24000))
 (lambda () (make-server 24001))
 (lambda () (make-server 24002)))

(pp server-clients)
;; (close-tcp-server-socket s)
