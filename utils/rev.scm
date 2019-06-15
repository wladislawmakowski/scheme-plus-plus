;revision

(define (evquote exp)
    (cadr exp))

(define (evcf exp env)
    (cond ((if? exp) (evif exp env))
          ((switch? exp) (evsw exp env))
          ((cond? exp) (evcnd (cdr exp) env))
          ((for? exp) (evfr exp env))
          ((while? exp) (evwh exp env))
          (else (printf "ILLEGAL CONTROL FLOW OPERATOR: ~a" (car exp)))))

(define (evif exp env)
    (if (true? (eval:: (predicate exp) env))
        (eval:: (consequent exp) env)
        (eval:: (alternative exp) env)))

(define (if_predicate exp)
    (cadr exp))

(define (if_consequent exp)
    (caddr exp))

(define (if_alternative exp)
    (cdddr exp))

(define (evsw exp env)
    (evsw_dis (sw-arg exp) (sw-cases exp) env))

(define (evsw_dis arg exp env)              ; exp = (list <cases>)
    (if (null? (cdr exp))
        '()
        (if (eq? (evcase arg (car exp) env) 'no)
            (evsw_dis arg (cdr exp) env)
            '())))

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

(define (evwh exp env)
    (cond ((eq? (condition exp) 'true) (evwh exp env))
          ((eq? (condition exp) 'false) (evwh exp env))
          ((number? (condition exp)) (evwh exp env)))
    (if (false? (condition exp))
        '()
        (begin (eval:: (statement exp) env)
               (evwh exp env))))

(define (condition exp)
    (cadr exp))

(define (statement exp)
  (cddr exp))

(define (evcnd exp env)
  (if (true? (cnd_predicate exp))
      (eval:: (cnd_consequent exp) env)
      (if (eq? (cnd_predicate exp) 'else::)
	  (eval:: (cnd_consequent exp) env)
	  (evcnd (cdr exp) env))))

(define (cnd_predicate exp)
  (caar exp))

(define (cnd_consequent exp)
  (cdar exp))


