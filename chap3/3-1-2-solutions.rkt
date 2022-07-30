#lang racket

(require math/base)

(define (random-in-range low high)
    (let ((range (- high low)))
    (+ low (* range (/ (random 100000) 100000.0)))))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0) (/ trials-passed trials))
              ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
              (else (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(define (rect-area x1 x2 y1 y2) (* (- x2 x1) (- y2 y1)))

(define (estimate-integral p? x1 x2 y1 y2 trials)

  (define (rand-x) (random-in-range x1 x2))
  (define (rand-y) (random-in-range y1 y2))
  (define (experiment) (p? (rand-x) (rand-y)))

  (* (rect-area x1 x2 y1 y2) (monte-carlo trials experiment)))

(define (estimate-pi)
  (define radius 3.0)
  (define (p? x y)
     (>= (sqr radius) (+ (sqr (- x 5))
                         (sqr (- y 7)))))

  (/ (estimate-integral p? 2 8 4 10 1000000) (sqr radius)))

(estimate-pi)
