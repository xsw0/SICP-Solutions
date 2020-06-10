#lang sicp
(define (count-pairs x)
  (let ((counted '()))
    (define (iter x)
      (if (or (not (pair? x))
              (memq x counted))
          0
          (begin (set! counted (cons x counted))
                 (+ 1
                    (iter (car x))
                    (iter (cdr x))))))
    (iter x)))

(define return3 (cons 1 (cons 2 (cons 3 '()))))
(define return4
  (let ((l (cons 1 '())))
    (cons l (cons l '()))))
(define return7
  (let ((l (cons 1 '())))
    (let ((l2 (cons l l)))
      (cons l2 l2))))
(define never-return
  (let ((l1 (cons 1 '()))
        (l2 (cons 2 '()))
        (l3 (cons 3 '())))
    (set-cdr! l1 l2)
    (set-cdr! l2 l3)
    (set-cdr! l3 l1)
    l1))

return3                               ; (1 2 3)
(count-pairs return3)                 ; 3
return4                               ; ((1) (1))
(count-pairs return4)                 ; 3
return7                               ; (((1) 1) (1) 1)
(count-pairs return7)                 ; 3
never-return
(count-pairs never-return)            ; 3
(count-pairs '(1 2 3 4 5 6 7 8 9 10)) ; 10