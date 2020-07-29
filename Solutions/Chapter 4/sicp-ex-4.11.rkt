#lang sicp

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
