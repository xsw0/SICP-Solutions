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

(define (display-inf-stream s low high)
  (display-stream
   (stream-map (lambda (i)
                 (stream-ref s i))
               (stream-enumerate-interval low high))))

(define (add-streams . args)
  (apply stream-map (cons + args)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (partial-sums s)
  (define ps (add-streams s (cons-stream 0 ps)))
  ps)

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (constant-stream c)
  (cons-stream c
               (constant-stream c)))

(define (sign-change-detector first second)
  (cond ((and (< first 0)
              (>= second 0))
         1)
        ((and (>= first 0)
              (< second 0))
         -1)
        (else 0)))

(define (stream-list list)
  (define (ll l)
    (if (null? l)
        (ll list)
        (cons-stream (car l)
                     (ll (cdr l)))))
  (ll list))

(define sense-data
  (stream-list
   '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cdr sense-data)))

(display-inf-stream zero-crossings 0 100)
