#lang sicp

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (inverter input output)
  (define (invert-output)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-output) 'ok)

(define (and-gate i1 i2 o)
  (define (and-gate-output)
    (let ((new-value (logical-and (get-signal i1)
                                  (get-signal i2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! o new-value)))))
  (add-action! i1 and-gate-output)
  (add-action! i2 and-gate-output)
  'ok)

(define (or-gate i1 i2 o)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
    (inverter i1 a)
    (inverter i2 b)
    (and-gate a b c)
    (inverter c o)))

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (is-binary? a)
  (cond ((= a 1) #t)
        ((= a 0) #t)
        (else #f)))

(define (logical-not s)
  (cond ((not (is-binary? s)) (error "Invalid signal"))
        ((= s 0) 1)
        ((= s 1) 0)
        (else (error "This case is not reachable."))))

(define (logical-and a b)
  (cond ((or (not (is-binary? a))
             (not (is-binary? b)))
         (error "Invalid signal"))
        ((= a b 1) 1)
        (else 0)))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))

    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procs)
  (if (null? procs)
      'done
      (begin ((car procs))
             (call-each (cdr procs)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (get-signal w) (w 'get-signal))
(define (set-signal! w v) ((w 'set-signal) v))
(define (add-action! w proc) ((w 'add-action!) proc))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))

(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segment agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belong-before? segments)
    (or (null? segments)
        (< time
           (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments))
           time)
        (insert-queue! (segment-queue (car segments))
                       action)

        (let ((rest (cdr segments)))
          (if (belong-before? segments)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belong-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((first-segment-queue (segment-queue (first-segment agenda))))
    (delete-queue! first-segment-queue)
    (if (empty-queue? first-segment-queue)
        (set-segments! agenda (rest-segment agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")

      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (make-queue) (list 'queue))
(define (empty-queue? q) (= (length q) 1))

(define (insert-queue! q v)
  (if (empty-queue? q)
      (set-cdr! q (cons v '()))
      (insert-queue! (cdr q) v)
      )
  )
(define (front-queue q) (car (cdr q)))

(define (delete-queue! q)
  (if (empty-queue? q)
      (error "Deleting from Empty Queue.")
      (set-cdr! q
                (cdr (cdr q)))))

(define the-agenda (make-agenda))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))

(and-gate input-1 input-2 output)
(set-signal! input-1 1)
(set-signal! input-2 1)
(get-signal output)
