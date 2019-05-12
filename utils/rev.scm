;revision

(define (evquote exp)
    (cadr exp))

(define (evcf exp)
    (cond ((if? exp) (evif exp))
          ((switch? exp) (evsw exp))
          ((cond? exp) (evcnd exp))
          ((for? exp) (evfr exp))
          ((while? exp) (evwh exp))
          (else (printf "ILLEGAL CONTROL FLOW OPERATOR: ~a" (car exp)))))

(define (evif exp)
    ())

(define (predicate exp)
    (cadr exp))

(define (consequent exp)
    (caddr exp))

(define (alternative exp)
    ())
