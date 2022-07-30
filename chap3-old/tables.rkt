#lang sicp

; 1D table

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? (caar records) key) (car records))
        (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value)
                      (cdr table))))))
(define (make-table)
  (list '*table*))

; 2-D table

(define (lookup2-D key-1 key-2 table)
  (let ((subtable
          (assoc key-1 (cdr table))))
    (if subtable
      (let ((record
              (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          #f)
        #f))))

(define (insert2-D! key-1 key-2 value table)
    (if subtable
      (insert! key-2 subtable)
      (set-cdr! table
        (cons (list key-1
                (cons key-2 value))
              (cdr table)))))
'ok)
