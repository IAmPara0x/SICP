#lang sicp

(define front-ptr car)
(define rear-ptr cdr)

(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "empty-queue")
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue) (set-front-ptr! queue new-pair)
                                (set-rear-ptr! queue new-pair)
                                queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (set-front-ptr! queue (cdr (front-ptr queue)))
  queue)

(define print-queue car)

