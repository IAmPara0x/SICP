#lang sicp

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a1 a2)
  (cond ((= a1 a2 1) 1)
        (else 0)))

(define (or-gate a1 a2 ouput)
  (let ((b1 (make-wire))
        (b2 (make-wire))
        (b3 (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 b3)
    (inverter b3 output)))

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s))
'ok)


(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
  (half-adder b c-in s c1)
  (half-adder a s sum c2)
  (or-gate c1 c2 c-out)
  'ok))

(define (ripple-carry-adder a_k b_k s_k C)
  (define (iter A B S c_in c_out)
    (if (null? A)
      S
      (begin
        (full-adder (car A) (car B) (car S) c_in c_out)
        (iter (cdr A) (cdr B) (cdr S) c_out (make-wire)))))
  (iter a_k b_k s_k C (make-wire)))

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
          ((eq? m 'set-signal!) set-my-signal!)
          ((eq? m 'add-action!) accept-action-procedure!)
          (else (error "Unknown operation"))))
dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin ((car procedures))
            (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedures)
  ((wire 'add-action) action-procedures))

; Agenda

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

