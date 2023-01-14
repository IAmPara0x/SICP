#lang racket

; (define [halt? p a] ....)

(define [run-forever] (run-forever))
(define [try p]
  (if (halts? p p) (run-forever) 'halted))

;; (try try)

#|

I: (try try)

What it the output of (try try)?
=> There are two cases either (try try) will
   1. run-forever
   2. 'halted

Suppose the output of (try try) is 'halted
then that means (halts? p p) should be true and (try try) should run-forever.

|#
