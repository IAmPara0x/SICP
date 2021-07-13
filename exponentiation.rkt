; -------------------------------------------

#|
  Exponentiation
  b^n = b * b^(n-1)
  b^0 = 1
|#

; Recursive Procedure
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

; Iterative Procedure

(define (expt-iter b1 b2 x n)
  (if (= n 0)
    b2
    (expt-iter (* b1 x) b1 x (- n 1))
  )
)

(define (exptI x n) (expt-iter x 1 x n))

; Recursive Procedure
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; Iterative Procedure

#|

|#

;TODO: complete this function
(define (fast-expt-iter))

; -------------------------------------------
