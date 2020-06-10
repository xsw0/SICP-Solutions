#lang sicp
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

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

return3               ; (1 2 3)
(count-pairs return3) ; 3
return4               ; ((1) (1))
(count-pairs return4) ; 4
return7               ; (((1) 1) (1) 1)
(count-pairs return7) ; 7
never-return
; (count-pairs never-return)