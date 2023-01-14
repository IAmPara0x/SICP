#lang racket

(require "meta-circular-evaluator.rkt")
(require "special-expressions.rkt")

(define input-prompt "input: ")
(define output-prompt "output: ")

(define [driver-loop]
  (display input-prompt)
  (let* ([input (read)]
         [output (eval input base-env)])
    (display output-prompt)
    (println output))
  (driver-loop))

[driver-loop]
