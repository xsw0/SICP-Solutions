#lang sicp

(define (permutation . args)
  (define (accumulate op lst inital)
    (if (null? lst)
        inital
        (op (car lst)
            (accumulate op (cdr lst) inital))))
  (define (falt-map proc . lst)
    (accumulate append (apply map (cons proc lst)) '()))
  (cond ((null? args) (list '()))
        ((null? (cdr args)) (list (list (car args))))
        (else
         (falt-map (lambda (l)
                     (define (all-insert l x)
                       (if (null? l)
                           (list (list x))
                           (cons (cons x l)
                                 (map (lambda (ll)
                                        (cons (car l) ll))
                                      (all-insert (cdr l) x)))))
                     (all-insert l (car args)))
                   (apply permutation (cdr args))))))

;a
(define balance '())
(map (lambda (proc-lst)
       (set! balance 100)
       (map (lambda (proc)
              (proc)
              (display balance)
              (newline))
            proc-lst)
       (newline))
     (permutation (lambda ()
                    (set! balance (+ balance 10)))
                  (lambda ()
                    (set! balance (- balance 20)))
                  (lambda ()
                    (set! balance (- balance (/ balance 2))))))
;b 6!/2!/2!/2! = 90
;b 7!/2!/2!/3! = 210
;b 9!/3!/3!/3! = 1680
