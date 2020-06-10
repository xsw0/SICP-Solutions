#lang sicp
(define (contains-cycle? l)
  (define encountered '())
  (define (iter l)
    (cond ((null? l) #f)
          ((memq l encountered) #t)
          (else (begin (set! encountered
                             (cons l encountered))
                       (iter (cdr l))))))
  (iter l))

(define (make-cycle x)
  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
  (set-cdr! (last-pair x) x)
  x)

(define t1 (list 'a 'b))
(define t2 (list t1 t1))

(contains-cycle? '())
(contains-cycle? '(1 2 3))
(contains-cycle? (make-cycle '(1)))
(contains-cycle? (make-cycle '(1 2 3)))
(contains-cycle? t1)
(contains-cycle? t2)