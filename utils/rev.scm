;revision

(define (evquote exp)
    (cadr exp))

(define (evcf exp env)
    (cond ((if? exp) (evif exp env))
          ((switch? exp) (evsw exp env))
          ((cond? exp) (evcnd exp env))
          ((for? exp) (evfr exp env))
          ((while? exp) (evwh exp env))
          (else (printf "ILLEGAL CONTROL FLOW OPERATOR: ~a" (car exp)))))

(define (evif exp env)
    (if (true? (eval:: (predicate exp) env))
        (eval:: (consequent exp) env)
        (eval:: (alternative exp) env)))

(define (predicate exp)
    (cadr exp))

(define (consequent exp)
    (caddr exp))

(define (alternative exp)
    (cadddr exp))

(define (evsw exp env)
    ())

(define (evsw_dis exp env)              ; exp = (list <cases>)
    ())

(define (sw-arg exp)
    (cadr exp))

(define (sw-cases exp)
    (cddr exp))

(define (evcase arg exp env)
    (if (eq? (car exp) 'case::)
        (if (eq? (cadr exp) arg)
            (eval:: (caddr exp) env)
            'no)
        'no)
    (if (eq? (car exp) 'default::)
        (eval:: (cadr exp) env)
        'no))
