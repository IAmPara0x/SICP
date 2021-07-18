
#|
Why it suffices to check sqrt of a number to determine it's primality?

let n be any natural number.

then, sqrt(n) * sqrt(n) = n

suppose we can factorize n => n = a * b

then following three cases can be formed
1. a < sqrt(n) and sqrt(n) < b
2. a > sqrt(n) and sqrt(n) > b
3. a = sqrt(n) and sqrt(n) = b

∴ if both a, b > sqrt(n) then a * b > n

----------

suppose n = 11, to check wheter it's a prime number or not.

we can start testing the divisibility of n starting with integer 2.
not 2 | 11

we can continue to test with next integer because 2^2 < 11
what this means is highest number we can get by multiplying two numbers is 2^2
but 11 is greater than that so we must multiply more bigger numbers to get 11

next number is 3
not 3|11

again we can continue te text with next integer because 3^3 < 11
with only 2 and 3 the highest number we can get by multiplying two numbers is 9
every other combination of the number will be <= 9.

Therefore, we must continue to test with divisibility of n with next integer ie. 4.
not 4|11

now, we can stop because 4^2 > 11.
what this means is we have tried every possible combination of numbers from 2 to 4 to multiply with,
and we exceeded our target is one combination multiplying any number greater than
4 will only result in numbers that are greater than n.
|#

; -------------------------------------------

(define (smallest-divisor n) (find-divisor n 2))

(define (next x)
  (if (= x 2)
    3
    (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


; -------------------------------------------

;Fermat's Test

#|
NOTE: proof fermat's little theorm.
|#

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

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


(define (fast-prime? n times)
  (cond ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

; -------------------------------------------
