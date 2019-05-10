(define (self-eval? exp)
    (cond ((number? exp) #t)
          ((string? exp) #t)
          (else #f)))

(define (variable? exp)
    (symbol? exp))

(define (quote? exp)
    (if (eq? (car exp) 'quote)
        #t
        #f))

(define (assignment? exp)
    (if (eq? (car exp) 'set!)
        #t
        #f))

(define (definition? exp)
    (if (eq? (car exp) 'define)
        #t
        #f))

(define (class-definition? exp)
    (if (eq? (car exp) 'define-class)
        #t
        #f))

(define (control-flow? exp)
    (cond ((if? exp) #t)
          ((switch? exp) #t)
          ((cond? exp) #t)
          ((for? exp) #t)
          ((while? exp) #t)
          (else #f)))

(define (lambda? exp)
    (if (eq? (car exp) 'lambda)
        #t
        #f))

(define (begin? exp)
    (if (eq? (car exp) 'begin)
        #t
        #f))

(define (application? exp)
    (pair? exp))
