
; (require math/number-theory)
; -------------------------------------------

; Higher order procedures

(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence) ; synonym fold-right
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; -------------------------------------------


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))


(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap
      (lambda (i)
        (map (lambda (j) (list i j))
             (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
    (map make-pair-sum
    (filter prime-sum? (unique-pairs n))))


; -------------------------------------------

(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
                (map (lambda (p) (cons x p))
                (permutations (remove x s))))
                s)))

(define (remove item sequence)
(filter (lambda (x) (not (= x item)))
sequence))

; -------------------------------------------

; Eight Queens Puzzle

; TODO: complete this puzzle ex 2.42




