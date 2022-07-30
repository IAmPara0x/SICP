#lang sicp


(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enum-interval lo hi)
  (if (> lo hi)
      the-empty-stream
      (cons-stream lo (stream-enum-interval (+ lo 1) hi))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (show x)
  (display-line x) x)


;; Helper procedures
;; -------------------------------------------

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

;; -------------------------------------------

(define (square x) (* x x))

(define (expmod base exp m)
 (cond ((= exp 0) 1)
       ((even? exp)
         (remainder
           (square (expmod base (/ exp 2) m))
           m))
       (else
         (remainder
           (* base (expmod base (- exp 1) m))
           m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (prime? n times)
  (cond ((= times 0) true)
    ((fermat-test n) (prime? n (- times 1)))
    (else false)))
;;-------------------------------------------
;; Ex: 3.51
;;
;; (define x (stream-map show (stream-enum-interval 0 10)))
;; (stream-ref x 1)
;; (stream-ref x 5)

;; Ex: 3.52
;;

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enum-interval 1 20)))

(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))

(stream-ref y 7)
sum
(display-stream z)
