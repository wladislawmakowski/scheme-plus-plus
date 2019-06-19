;verification

(define (self-eval? exp)
    (cond ((number? exp) #t)
          ((string? exp) #t)
          (else #f)))

(define (variable? exp)
    (symbol? exp))

(define (quote? exp)
    (if (eq? (car exp) 'quote::)
        #t
        #f))

(define (assignment? exp)
    (if (eq? (car exp) 'set::)
        #t
        #f))

(define (definition? exp)
    (if (eq? (car exp) 'define::)
        #t
        #f))

(define (class-definition? exp)
    (if (eq? (car exp) 'define_c::)
        #t
        #f))

(define (control-flow? exp)
    (cond ((if? exp) #t)
          ((switch? exp) #t)
          ((cond? exp) #t)
          ((for? exp) #t)
          ((while? exp) #t)
          (else #f)))

(define (if? exp)
    (if (eq? (car exp) 'if::)
        #t
        #f))

(define (switch? exp)
    (if (eq? (car exp) 'switch::)
        #t
        #f))

(define (cond? exp)
    (if (eq? (car exp) 'cond::)
        #t
        #f))

(define (for? exp)
    (if (eq? (car exp) 'for::)
        #t
        #f))

(define (while? exp)
    (if (eq? (car exp) 'while::)
        #t
        #f))

(define (lambda? exp)
    (cond ((eq? (car exp) 'λ) #t)
          ((eq? (car exp) 'lambda::) #t)
          (else #f)))

(define (eval_s? exp)
    (if (eq? (car exp) 'eval_s::)
        #t
        #f))

(define (application? exp)
  (pair? exp))

(define (compound? exp)
  (if (eq? (car exp) 'proc_xcmpd#1)
      #t
      #f))
