#lang sicp

;; Tables


(define (make-table same-key?)

  (define (assoc key records)

    (define (assoc-eq? pair)
        (same-key? (car pair) key))

    (cond ((null? records) #f)
            ((assoc-eq? (car records)) (car records))
            (else (assoc key (cdr records)))))


  (let ((local-table (list '*table*)))

    (define (lookup keys)
      (cond ((null? keys)
             (error "No keys provided for the lookup."))

            ((null? (cdr keys))
             (let ((record (assoc (car keys) (cdr local-table))))
               (if record (cdr record) #f)))

            (else
             (let ((subtable (assoc (car keys) (cdr local-table)))
                   )
               (if subtable
                   (((cdr subtable) 'lookup-proc) (cdr keys))
                   #f
                   )
               )
             )
            )
      )

    (define (insert! keys value)
      (cond ((null? keys)
             (error "No keys provided for inserting."))
            ((null? (cdr keys))
             (let ((record (assoc (car keys) (cdr local-table))))
               (if record
                   (set-cdr! record value)
                   (set-cdr! local-table
                             (cons (cons (car keys) value) (cdr local-table))))))
            (else
             (let ((subtable (assoc (car keys) (cdr local-table))))
               (if subtable
                   (((cdr subtable) 'insert-proc!) (cdr keys) value)

                   (let ((new-table (make-table same-key?)))

                     ((new-table 'insert-proc!) (cdr keys) value)
                     (set-cdr! local-table
                               (cons (cons (car keys) new-table)
                                     (cdr local-table)))))))))

    (define (print-table table)

      (define (print-pair pair)
          (display "(")
          (display (car pair))
          (display ", ")
          (if (procedure? (cdr pair))
              (print-table ((cdr pair) 'get))
              (display (cdr pair)))
          (display ")")
          )


      (define (print-pairs pairs)
          (cond ((null? (cdr pairs)) (print-pair (car pairs)))
              (else (print-pair (car pairs))
                      (display ", ")
                      (print-pairs (cdr pairs)))))

      (cond ((null? (cdr table)) (display "<empty table>"))
              (else (display "[")
                  (print-pairs (cdr table))
                  (display "]"))))

    (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              ((eq? m 'print) (print-table local-table))
              ((eq? m 'get) local-table)
              (else (error "Unknown operation: TABLE" m))))
    dispatch))




(define (insert! keys v t) ((t 'insert-proc!) keys v))
(define (lookup keys t) ((t 'lookup-proc) keys))
(define (print-table t) (t 'print))

(define t1 (make-table eq?))

(insert! (list "a" "a" "a") 0 t1)
(insert! (list "a" "a" "b") 10 t1)
(insert! (list "b" "b" "b") 1 t1)
(insert! (list "b" "b" "c") 2 t1)
(insert! (list "b" "b" "w" "c") 2 t1)

(print-table t1)

(newline)
(lookup (list "a" "a" "a") t1)
(lookup (list "a" "a" "b") t1)
