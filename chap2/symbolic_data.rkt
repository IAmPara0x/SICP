
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (equal? a b)
  (if (and (null? a) (null? b))
    #t
    (cond
      ((or (null? a) (null? b)) #f)
      ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (else #f)
    )
  )

)

(equal? '(this a) '(this a))

