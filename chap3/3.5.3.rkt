#lang racket

(define (force exp) (exp))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define [memo-proc exp]
    (let ([already-run? #f] [result #f])
      (lambda []
        (if already-run?
            result
            (begin
                (set! already-run? #t)
                (set! result (exp))))
        result)))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ x xs) (cons x (delay xs)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define stream-null? null?)

(define empty-stream '())



;; Utility functions
(define (stream-take! n stream)
  (if (= n 0)
      empty-stream
      (cons (stream-car stream)
            (stream-take! (- n 1) (stream-cdr stream)))))

(define (stream-ref n stream)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (- n 1) (stream-cdr stream) )))

(define (any pred? xs)
  (cond [(null? xs) #f]
        [(pred? (car xs)) #t]
        [else (any pred? (cdr xs))]))

(define [stream-map proc . streams]
  (if (any stream-null? streams)
      empty-stream
      (cons-stream (apply proc (map stream-car streams))
                   (apply stream-map (cons proc (map stream-cdr streams))))))

(define [stream-filter pred? stream]
  (cond [(stream-null? stream) empty-stream]
        [(pred? (stream-car stream)) (cons-stream (stream-car stream)
                                                  (stream-filter pred? (stream-cdr stream)))]
        [else (stream-filter pred? (stream-cdr stream))]))

(define [partial-sums stream]

  (let ([x (stream-car stream)]
        [xs (stream-cdr stream)]
        )
  (cons-stream x
               (stream-map (λ [n] (+ n x)) (partial-sums xs)))))

(define integers (cons-stream 1 (stream-map (λ [x] (+ x 1)) integers)))

(define natsum (partial-sums integers))

(define [avg . xs] (/ (foldr + 0 xs) (length xs)))

(define [sqrt-improve guess x]
  (avg guess (/ x guess)))

(define [sqrt-stream x]
  (define guesses
    (cons-stream 1.0
                 (stream-map (λ [guess] (sqrt-improve guess x)) guesses)))
  guesses)


(define [is-even? x] (= (remainder x 2) 0))
(define [is-odd? x] (not (is-even? x)))

(define [reciprocal x] (/ 1.0 x))

(define [foldl-stream f b as]

  (cond [(stream-null? as) b]
        [else (foldl-stream f
                            (f (stream-car as) b)
                            (stream-cdr as))]))

(define (pi-summands)

  (define [f x]
    (cond [(= (remainder x 4.0) 3.0) (- (reciprocal x))]
          [else (reciprocal x)]))

  (stream-map f (stream-filter is-odd? integers)))

(define pi-stream
  (stream-map (λ [s] (* 4.0 s))
              (partial-sums (pi-summands))))

(define [square x] (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
  (cons-stream (- s2 (/ (square (- s2 s1))
               (+ s0 (* -2 s1) s2)))
               (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;; TODO: This doesn't work. Figure out why.
(define accelerated-pi-stream (make-tableau euler-transform pi-stream))



#|
(define [sqrt-stream x]
  (define guesses
    (cons-stream 1.0
                 (stream-map (λ [guess] (sqrt-improve guess x)) guesses)))
  guesses)

(define [sqrt-stream x]
    (cons-stream 1.0
                 (stream-map (λ [guess] (sqrt-improve guess x)) (sqrt-stream x)))
  )

x1 = (1.0 thunk)

evaluate the thunk

(stream-map (λ [guess] (sqrt-improve guess x)) x1) => (f(1.0) thunk)

evaluate the thunk

(stream-map (λ [guess] (sqrt-improve guess x) (stream-cdr x1)))
(f(1.0) thunk)

(f(1.0) (cons-stream 1))
|#

(define [integer-starting-from n]
  (cons-stream n (integer-starting-from (+ n 1))))

(define [is-prime? primes n]

  (define [divides? x y] (= (remainder x y) 0))
  (define [square x] (* x x))

  (let ([p (stream-car primes)])
    (cond [(< n (square p)) #t]
          [(divides? n p) #f]
          [else (is-prime? (stream-cdr primes) n)]
          )
   )
  )

(define primes
  (cons-stream 2
               (stream-filter (λ [n] (is-prime? primes n)) (integer-starting-from 3))
               ))

(define [mk-pair x y] (list x y))

(define [inc x] (+ 1 x))

#|
My implementation: 

(define [interleave-stream s1 s2]
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else (cons-stream (stream-car s1)
                           (cons-stream (stream-car s2)
                                        (interleave-stream (stream-cdr s1) (stream-cdr s2))))]))

(define [pairs s t]
  (cons-stream (mk-pair s t)
               (interleave-stream (stream-map (λ [y] (mk-pair s y)) (integer-starting-from (inc t)))
                                  (pairs (inc s) (inc t))
                                  )
               )
  )
|#

;; (define [pairs s t]
;;   (cons-stream (mk-pair (stream-car s) (stream-car t))
;;                (interleave
;;                 (stream-map (λ [y] (mk-pair (stream-car s) y)) (stream-cdr t))
;;                 (pairs (stream-cdr s) (stream-cdr t)))))

(define [interleave . streams]

  (let ([s1 (car streams)])
    (cond [(stream-null? s1) (apply interleave (stream-cdr streams))]
          [else
            (cons-stream (stream-car s1)
                        (apply interleave (append (cdr streams) (list (stream-cdr s1)))))])))

(define [dec x] (- x 1))

#|
Ex: 3.66

TODO: improve the efficiency.
|#

(define [pairs-idx x y]
  (cond [(= x y 1) 0]
        [(= x (dec y)) (+ (expt 2 (dec x)) (pairs-idx x x))]
        [(= x y) (+ (expt 2 (dec x)) (pairs-idx (dec x) (dec y)))]
        [else (+ (* (expt 2 x) (+ y (- x) -1))
                 (pairs-idx x (inc x)))]))

#|
Ex: 3.67
|#

(define [pairs s t]
  (cons-stream (mk-pair (stream-car s) (stream-car t))
               (interleave
                (stream-map (λ [x] (mk-pair x (stream-car t))) (stream-cdr s))
                (stream-map (λ [y] (mk-pair (stream-car s) y)) (stream-cdr t))
                (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs
  (stream-filter (λ [p] (apply <= p)) (pairs integers integers)))

(define int-primes
  (stream-filter (λ [p] (is-prime? primes (apply + p)))
                 (stream-filter (λ [p] (apply <= p)) (pairs integers integers))))

(define [merge-weighted weight s1 s2]
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
         (let* ([x1 (stream-car s1)]
                [weight-x1 (weight x1)]
                [x2 (stream-car s2)]
                [weight-x2 (weight x2)])

           (cond [(= weight-x1 weight-x2)
                  (cons-stream x1 (merge-weighted weight (stream-cdr s1) (stream-cdr s2)))]
                 [(< weight-x1 weight-x2)
                  (cons-stream x1 (merge-weighted weight (stream-cdr s1) s2))]
                 [else
                  (cons-stream x2 (merge-weighted weight s1 (stream-cdr s2)))]
                 ))
         ]
        ))

(define [weighted-pairs weight s t]
  (cons-stream (mk-pair (stream-car s) (stream-car t))
               (merge-weighted
                    weight
                    (stream-map (λ [y] (mk-pair (stream-car s) y)) (stream-cdr t))
                    (pairs (stream-cdr s) (stream-cdr t)))))

(define 3.70.a (weighted-pairs (λ [p] (apply + p)) integers integers))

