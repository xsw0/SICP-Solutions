#lang sicp
(define (or-gate a1 a2 output)
  (let ((n1 (make-wire))
        (n2 (make-wire))
        (na (make-wire)))
    (inverter a1 n1)
    (inverter a2 n2)
    (and-gate n1 n2 na)
    (inverter na output)))
