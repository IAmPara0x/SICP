
; -------------------------------------------

; Compound procedures in picture lang
#|

Creating compound procedures in this picture lang is possible,
because painter satisfies closure property.

Consider like the for better understanding

suppose all numbers are painters
then numbers such as 1 2 3 4 etc are painters

using compound procedure on these numbers will produce just another number on which we can all the compound procedures,
that we could have used previously.

for eg
(define (pow a b) a^b)

is a compound procedure using any two number in pow procedure will procedure just another number.
Therefore we can build complex procedures such as

(define (a+b)^2 ((pow a 2) + 2*a*b + (pow b 2)))

but consider this if pow procedure would have produced something that was not a number then
this new complex procedure becomes invalid.
|#

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

; -------------------------------------------

; Abstracting patterns in picture lang
#|

|#

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

; -------------------------------------------
; Recursive procedures in picture lang

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let (
          (up (up-split painter (- n 1)))
          (right (right-split painter (- n 1)))
          (corner (corner-split painter (- n 1)))
          )
        (let (
              (top-left (beside up up))
              (bottom-right (below right right))
              )
          (below (beside top-left corner) (beside painter bottom-right))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half))))

; -------------------------------------------

;Higher order procedures

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
      (bottom (beside (bl painter) (br painter))))
    (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((combine4 (square-of-four flip-horiz identity
                                    rotate180 flip-vert)))
      (combine4 quarter))))


(define (split t1 t2)
  (define (splitA painter n)
    (if (= n 0)
      painter
      (let (
            (smaller (splitA painter (- n 1)))
            )
        (t1 painter (t2 smaller smaller)))))
  splitA)

(define right-split (split beside below))
(define up-split (split below beside))

; Frames

;Vector operations

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (vect-op op)
  (lambda (v1 v2)
    (make-vect (op (xcor-vect v1) (xcor-vect v2))
               (op (ycor-vect v1) (ycor-vect v2)))))

(define add-vect (vect-op +))
(define sub-vect (vect-op -))
(define scale-vect
  (lambda (n v)
    ((vect-op *) (make-vect n n) v)))

(define (make-frame origin edge1 egde2)
  (list origin edge1 edge1))

(define origin-frame car)
(define egde1-frame cadr)
(define egde2-frame caddr)


(define (frame-coord-map a-frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (egde1-frame frame))
                        (scale-vect (ycor-vect v) (egde2-frame frame))))))

