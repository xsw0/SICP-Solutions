#lang sicp
(define (contains-cycle? l)
  (define end l)
  (define (find? begin end x)
    (cond ((eq? begin end) #f)
          ((eq? begin x) #t)
          (else (find? (cdr begin) end x))))
  (define (iter x)
    (cond ((null? x) #f)
          ((eq? x (cdr x)) #t)
          ((find? l end x) #t)
          (else (set! end x)
                (iter (cdr x)))))
  (iter l))

; Tested with mzscheme implementation of R5RS:
(define x '(1 2 3 4 5 6 7 8))
(define y '(1 2 3 4 5 6 7 8))
(set-cdr! (cdddr (cddddr y)) (cdddr y))
(define z '(1))
(set-cdr! z z)
x ; (1 2 3 4 5 6 7 8)
y ; (1 2 3 . #0=(4 5 6 7 8 . #0#))
z ; #0=(1 . #0#)
(contains-cycle? x) ; #f
(contains-cycle? y) ; #t
(contains-cycle? z) ; #t


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
