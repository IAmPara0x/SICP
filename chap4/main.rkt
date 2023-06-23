#lang racket

(require "meta-circular-evaluator.rkt")
(require "special-expressions.rkt")

(define input-prompt "input: ")
(define output-prompt "output: ")

(define [driver-loop]
  (display input-prompt)
  (let* ([input (read)]
         [output ((analyze input) base-env)])
    (println input)
    (display output-prompt)
    (println output)
    )
  (driver-loop))

(define [newline? char] (eq? char #\newline))

(define [eval-file filename]
  (let ([content (file->value filename)]
        [start-time   (current-inexact-milliseconds)])
    (println ((analyze content) base-env))
    (println (/ (- (current-inexact-milliseconds) start-time) 1000))))
