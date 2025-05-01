(define board-size 5)

(define (initialize-board)
  (map (lambda (_) (make-list board-size "-")) (make-list board-size "-")))

(define (list-set! list k val)
    (if (zero? k)
        (set-car! list val)
        (list-set! (cdr list) (- k 1) val)))

(define (place-symbol symbol x y board)
  (let ((row (list-ref board x)))
    (if (string=? (list-ref row y) "-")
        (begin
          (list-set! row y symbol)
          #t)
        (error "error: board position already occupied:" (list x y)))))

(define (any? pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (any? pred (cdr lst)))))

(define (every? pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (every? pred (cdr lst)))
        (else #f)))

(define (check-line line)
  (let ((first (car line)))
    (if (and (not (string=? first "-"))
             (every? (lambda (elem) (string=? elem first)) line))
        first
        #f)))

(define (check-lines lines)
  (let loop ((lst lines) (result #f))
    (cond
      ((null? lst) result)
      ((not (eq? result #f)) result)
      (else
       (let ((winner (check-line (car lst))))
         (if (not (eq? winner #f))
             winner
             (loop (cdr lst) result)))))))

(define (check-win board)
  (define (check-rows) (check-lines board))

  (define (check-columns) (check-lines (apply map list board)))

  (define (check-diagonal board)
    (map (lambda (i) (list-ref (list-ref board i) i)) (range board-size)))

  (define (check-antidiagonal board)
    (map (lambda (i) (list-ref (list-ref board i) (- (- board-size 1) i))) (range board-size)))
  
  (or (check-rows) (check-columns) (check-line (check-diagonal board)) (check-line (check-antidiagonal board))))

(define (range n)
  (let loop ((i 0) (lst '()))
    (if (< i n)
        (loop (+ i 1) (cons i lst))
        (reverse lst))))

(define (print-board board)
  (for-each (lambda (row) (display row) (newline)) board))

(define board (initialize-board))

(place-symbol "O" 0 0 board)
(place-symbol "O" 1 1 board)
(place-symbol "O" 0 1 board)
(place-symbol "O" 0 2 board)
(place-symbol "O" 0 3 board)
(place-symbol "O" 0 4 board)
(place-symbol "X" 2 1 board)
(place-symbol "X" 3 1 board)
(place-symbol "O" 4 1 board)
(place-symbol "O" 2 2 board)
(place-symbol "O" 3 3 board)
(place-symbol "O" 4 4 board)
(place-symbol "X" 1 3 board)
