#lang sicp

(define x (list 1 2 3 4 5))

(define nil '())

(define (last-elem x)
  (cond ((eq? x nil) (error "list is empty"))
        ((null? (cdr x)) (car x))
        (else (last-elem (cdr x)))))

;; Queue

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons nil nil))

(define (front-queue queue)
  (if (eq? (front-ptr queue) nil)
      (error "The queue is empty.")
      (car (front-ptr queue))))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (insert-queue! queue item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)))
    (print-queue queue)
    )
  )

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "deleting from empty queue."))
        ((eq? (front-ptr queue) (rear-ptr queue))
         (set-front-ptr! queue nil) (set-rear-ptr! queue nil))
        (else (set-front-ptr! queue (cdr (front-ptr queue))))
        )
  (print-queue queue)
  )

;; Ex 3.21
(define (print-queue queue)
  (define (print-elems xs)
    (cond ((null? xs)
           (display ""))
          ((null? (cdr xs))
           (display (car xs)))
          (else
           (display (car xs)) (display "->") (print-elems (cdr xs)))))

  (cond ((null? (front-ptr queue)) (display ""))
        (else (print-elems (front-ptr queue)))
        )(newline))

(define q1 (make-queue))

(insert-queue! q1 'a)

(insert-queue! q1 'b)
(insert-queue! q1 'c)
(insert-queue! q1 'd)
(delete-queue! q1)

;; Queue with local state

;; Ex 3.22
(define (make-queue/v2)
  (let ((front-ptr nil)
        (rear-ptr nil))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ))
    dispatch))

;; Ex 3.23
(define (make-dequeue) (cons nil nil))
(define (make-dequeue-elem x) (list nil x nil))
(define (get-dequeue-elem e) (car (cdr e)))

(define (fst-dequeue-elem-ptr elem) (car elem))
(define (snd-dequeue-elem-ptr elem) (car (cdr (cdr elem))))

(define (set-fst-dequeue-elem-ptr! elem item) (set-car! elem item))
(define (set-snd-dequeue-elem-ptr! elem item) (set-car! (cdr (cdr elem)) item))

(define (dequeue-front dq) (car dq))
(define (dequeue-rear dq) (cdr dq))

(define (set-dequeue-front! dq item) (set-car! dq item))
(define (set-dequeue-rear! dq item) (set-cdr! dq item))


(define (print-dequeue dq)
  (define (print-elems xs)
    (cond ((null? (snd-dequeue-elem-ptr xs))
           (display (get-dequeue-elem xs)))
          (else
           (display (get-dequeue-elem xs)) (display "<->") (print-elems (snd-dequeue-elem-ptr xs)))
          )
    )

  (cond ((empty-dequeue? dq) (display "<empty deque>"))
        (else (print-elems (dequeue-front dq)))
        )(newline)
  )

(define (empty-dequeue? dq)
  (and (null? (dequeue-front dq))
       (null? (dequeue-rear dq))))

(define (front-insert-dequeue! dq item)
  (let ((new-item (make-dequeue-elem item)))

    (cond ((empty-dequeue? dq)
           (set-dequeue-front! dq new-item)
           (set-dequeue-rear! dq new-item)
           )

          (else   (set-snd-dequeue-elem-ptr! new-item (dequeue-front dq))
                  (set-fst-dequeue-elem-ptr! (dequeue-front dq) new-item)
                  (set-dequeue-front! dq new-item))
            )
    (print-dequeue dq)))

(define (rear-insert-dequeue! dq item)
  (let ((new-item (make-dequeue-elem item)))

    (cond ((empty-dequeue? dq)
            (set-dequeue-front! dq new-item)
            (set-dequeue-rear! dq new-item))

            (else (set-snd-dequeue-elem-ptr! (dequeue-rear dq) new-item)
                  (set-fst-dequeue-elem-ptr! new-item (dequeue-rear dq))
                  (set-dequeue-rear! dq new-item))
            )
    (print-dequeue dq)))


(define (front-delete-dequeue! dq)
  (cond ((empty-dequeue? dq) ("Deleting from empty dequeue."))
        ((eq? (dequeue-front dq) (dequeue-rear dq))
         (set-dequeue-front! dq nil)
         (set-dequeue-rear! dq nil))
        (else (set-dequeue-front! dq (snd-dequeue-elem-ptr (dequeue-front dq))))
    )(print-dequeue dq))

(define (rear-delete-dequeue! dq)
  (cond ((empty-dequeue? dq) ("Deleting from empty dequeue."))
        ((eq? (dequeue-front dq) (dequeue-rear dq))
         (set-dequeue-front! dq nil)
         (set-dequeue-rear! dq nil))
        (else (set-dequeue-rear! dq (fst-dequeue-elem-ptr (dequeue-rear dq)))
              (set-snd-dequeue-elem-ptr! (dequeue-rear dq) nil)
              )
    )(print-dequeue dq))


(display "Dequeue")
(newline)

(define dq1 (make-dequeue))
(front-insert-dequeue! dq1 'b)
(rear-insert-dequeue! dq1 'c)
(front-insert-dequeue! dq1 'a)
(rear-insert-dequeue! dq1 'd)
(rear-insert-dequeue! dq1 'e)
(front-delete-dequeue! dq1)
(rear-delete-dequeue! dq1)

