#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (take xs n)
  (if (= n 0)
    '()
    (cons (car xs) (take (force (cdr xs)) (- n 1)))))

;; -------------------------------------------

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers
  (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

;; Ex 3.54

(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials integers)))

;; Solution (take factorials 10)

;; -------------------------------------------
;; Ex 3.55
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (partial-sums stream)
  (define calc
    (cons-stream (car stream) (add-streams calc (stream-cdr stream))))
calc)

;; Solution (take (partial-sums integers) 10)

;; -------------------------------------------
;; Ex 3.56
;; NOTE: WHAT A ELEGANT SOLUTION.

(define s2 (scale-stream integers 2))
(define s3 (scale-stream integers 3))
(define s5 (scale-stream integers 5))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream
                    s2car
                    (merge s1 (stream-cdr s2))))

                  (else
                   (cons-stream
                    s1car
                    (merge (stream-cdr s1)
                           (stream-cdr s1)))))))))

(define S (cons-stream 1 (merge s5 (merge s2 s3))))

;; -------------------------------------------
;; Ex: 3.58
;;

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; Solution (take (expand 1 7 10) 10)
;; Solution (take (expand 3 8 10) 10)

;; -------------------------------------------
;; Ex 3.59
;; a)

(define (integrate-series stream)
  (mul-streams (stream-map (lambda (x) (/ 1 x)) integers)
               stream))

;; b)
;;

(define exp-series (cons-stream 1 (integrate-series exp-series)))
