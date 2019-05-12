;

(define (true? exp)
    (eq? exp #t))

(define (false? exp)
    (not (eq? exp #t)))
