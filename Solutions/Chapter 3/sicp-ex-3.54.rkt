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

;: (define x (stream-map show (stream-enumerate-interval 0 10)))
;: (stream-ref x 5)
;: (stream-ref x 7)


;;;SECTION 3.5.2

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; EXERCISE 3.53
(define s (cons-stream 1 (add-streams s s)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (display-inf-stream s low high)
  (display-stream
   (stream-map (lambda (i)
                 (stream-ref s i))
               (stream-enumerate-interval low high))))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials
                              (stream-cdr integers))))

(display-inf-stream factorials 0 10)
