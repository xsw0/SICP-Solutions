#lang sicp

;;;SECTION 3.5
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map 
              (cons proc (map stream-cdr argstreams))))))


;;;SECTION 3.5.1

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; force would normally be built into
;;  the stream implementation
;: (define (force delayed-object)
;:   (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))


;; EXERCISE 3.51

(define (show x)
  (display-line x)
  x)

;; EXERCISE 3.53

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (partial-sums s)
  (define ps (add-streams s (cons-stream 0 ps)))
  ps)

(define (display-inf-stream s low high)
  (display-stream
   (stream-map (lambda (i)
                 (stream-ref s i))
               (stream-enumerate-interval low high))))

(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) tolerance)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)))

(define (euler-transform s)
  (define (square x) (* x x))
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;;; Infinite streams of pairs

;: (stream-filter (lambda (pair)
;:                  (prime? (+ (car pair) (cadr pair))))
;:                int-pairs)

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave . s)
  (define interleave-stream
    (if (null? s)
        the-empty-stream
        (let ((first (car s)))
          (if (stream-null? first)
              (apply interleave (cdr s))
              (cons-stream (stream-car first)
                           (apply interleave
                                  (append (cdr s)
                                          (list (stream-cdr first)))))))))
  interleave-stream)

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples S T U)
  (define triples-stream
    (cons-stream (list (stream-car S)
                       (stream-car T)
                       (stream-car U))
                 (interleave (stream-map (lambda (x)
                                           (list (stream-car S)
                                                 (stream-car T)
                                                 x))
                                         (stream-cdr U))
                             (stream-map (lambda (p)
                                           (cons (stream-car S) p))
                                         (pairs (stream-cdr T)
                                                (stream-cdr U)))
                             (triples (stream-cdr S)
                                      (stream-cdr T)
                                      (stream-cdr U)))))
  triples-stream)

(display-inf-stream (stream-filter (lambda (t)
                                     (= (+ (* (car t)
                                              (car t))
                                           (* (cadr t)
                                              (cadr t)))
                                        (* (caddr t)
                                           (caddr t))))
                                   (triples integers integers integers))
                    0
                    4)