;;;; Procedures for server-client communcation:
;;;; make and receive requests of the form we're going to use.
;; You can load all definitions by using (load "requests.scm")

(define x 100)

(define (set-x! nval)
  (set! x nval)
  nval)

(set-x! 3)

(define (sleep-seconds seconds)
  (let ((start-time (get-universal-time)))
    (let loop ()
      (if (< (- (get-universal-time) start-time) seconds)
          (loop)))))

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
