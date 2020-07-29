#lang sicp

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (if (and (null? variables)
           (null? values)
           '())
      (cons (cons (car variables)
                  (car values))
            (make-frame (cdr variables)
                        (cdr values)))))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable var env)
  (define (env-loop env)
    (define (scan frame)
      (if (null? frame)
          (env-loop (enclosing-environment env))
          (let ((k-v (car frame)))
            (if (eq? var (car k-v))
                k-v
                (scan (cdr frame))))))
    (if (eq? env the-empty-environment)
        'Unbound-variable
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (lookup-variable-value var env)
  (let ((k-v (lookup-variable var env)))
    (if (eq? k-v 'Unbound-variable)
        (error "Unbound variable" var)
        (cdr k-v))))

(define (set-variable-value! var val env)
  (let ((k-v (lookup-variable var env)))
    (if (eq? k-v 'Unbound-variable)
        (error "Unbound variable -- SET!" var)
        (set-cdr! k-v val))))

(define (define-variable! var val env)
  (let ((frame (first-frame env))
        (k-v (lookup-variable var env)))
    (if (eq? k-v 'Unbound-variable)
        (add-binding-to-frame! var val frame)
        (set-cdr! k-v val))))
