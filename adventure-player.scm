;;;; Code for the adventure game player.
(load "../sdf/manager/load")
(manage 'new 'user-defined-types)
(load "client")


;; User interface
(define client-port)

(define (begin-sync port-num)
  (set! client-port (start-client port-num))
  'connected)

(define (run-in-sync thunk)
  (initiate-request client-port thunk))

#|
;; Testing
(begin-sync 21245)

;; client 1
(run-in-sync (lambda () (start-adventure 'ip10)))

(get-health my-avatar)
(get-location my-avatar)
(get-techcash my-avatar)

(go 'up)
(go 'down)
(go 'west)
(go 'east)
(go 'south)
(go 'north)
(go 'skew)
(go 'in)
(go 'out)
(whats-here)

(look-in-bag 'alyssa-hacker) ; empty :(

(close-port client-port)
