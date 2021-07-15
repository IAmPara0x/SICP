
; -------------------------------------------
#|
GCD: Euclid's Algorithm.

GCD(A,B) = GCD(B,r) where A = qB + r

Proof: GCD(A,B) = GCD(B, r)

D := GCD(A,B)
∴ D|A , D|B, D|(mA + nB)

hence D|(A - qB) => D|r

now C be a common divisor of r,B .

prove C <= D

hence C|r, C|B.

∴ C| qB + r => C|A

hence C divides A, B and r but D is the of GCD(A,B).

∴ C <= D

Order of growth = Θ(log n)

--------------
Hence GCD(A,B) = GCD(B,r) proved.
--------------
|#
; -------------------------------------------

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))
    )
  )
