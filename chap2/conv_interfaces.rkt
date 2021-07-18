
; -------------------------------------------
; higher order list procedures

(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; -------------------------------------------

#|
Before Abstraction

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
        (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
        (sum-odd-squares (cdr tree))))))
|#
; After Abstraction

(define (sum-odd-squares tree)
  (accumulate + 0 (map sqr (filter odd? (enumerate-tree tree)))))

#|
Before Abstraction

(define (even-fibs n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))
|#

;After Abstraction

(define (even-fibs n)
  (accumulate cons nil (filter even? (map fib (enumerate-interval 0 n)))))


; -------------------------------------------

#|
Creating only two higher order procedures ie filter and accumulate,
Can create show many procedures like map, append, etc.

I never even thought that procedures like map and append can be formulated using only accumulate.

(1 (2 3) (4 5))
|#

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (horner-eval x coeff-seq)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x))) 0 coeff-seq))


(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (not (pair? x))
                       1
                      (count-leaves x))) t)))

(count-leaves (list 1 (list (list 2 3) 5 6)))


