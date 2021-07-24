
; -------------------------------------------
; Higher order map procedure on trees.

(define nil '())


(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))


(define (tree-map f tree)
  (map (lambda (sub-tree)
    (if (pair? sub-tree)
      (tree-map f sub-tree)
      (f sub-tree)))
      tree))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
    (if (pair? sub-tree)
      (scale-tree sub-tree factor)
      (* sub-tree factor)))
      tree))

(define (sqaure-tree tree) (tree-map sqr tree))


#|

Subsets Explanation.

subset of list with 1 element is
(x) ()

subset of list with 2 element (x y) is

let list of subset of 1st element of given set be := X

then list of subset with 2 elements is X + ((append x' y) ∀,  x' ∈ X)

which will be

 = ((x) () (append (x) y) (append () y))
 = ((x) () (x y) (y))

and so . . .

|#
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest
              (map (lambda (z) (cons (car s) z)) rest)))))



