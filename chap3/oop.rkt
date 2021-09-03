#lang sicp


(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; A basic algorithm the checks if the list is cycled
(define (check-cycle x)
  (define (helper y)
    (cond
      ((not (pair? y)) #f)
      ((eq? x (cdr y)) #t)
      (else (helper (cdr y)))))
  (helper x))

; (define x '(1 2 3 4 5 6 7 8))
; (define y '(1 2 3 4 5 6 7 8))
; (set-cdr! (cdddr (cddddr y)) (cdddr y))
; (define z '(1))
; (set-cdr! z z)

(define z (make-cycle (list 'a 'b 'c)))

; (check-cycle x)
; (check-cycle y)
(check-cycle z)
