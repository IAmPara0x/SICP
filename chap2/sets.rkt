
; -------------------------------------------
#|
  Unordered representation of sets
|#

(define nil '())

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))


(define (intersection set1 set2)
  (cond
    ((or (null? set1) (null? set2)) nil)
    ((element-of-set? (car set1) set2)
     (cons (car set1) (intersection (cdr set1) set2)))
    (else (intersection (cdr set1) set2))))

(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    ((not (element-of-set? (car set1) set2))
     (cons (car set1) (union (cdr set1) set2)))

    (else (union (cdr set1) set2))))


; -------------------------------------------
#|
  Ordered representation of sets
|#

(define (element-of-Oset? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-Oset? x (cdr set)))))

(define (intersection-Oset set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
        (cons x1 (intersection-Oset (cdr set1)
                                   (cdr set2))))
        ((< x1 x2)
          (intersection-Oset (cdr set1) set2))
        ((< x2 x1)
          (intersection-Oset set1 (cdr set2)))))))


; -------------------------------------------
#|
  binary tree representation of sets
|#

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-Tset? x set)
  (cond
    ((null? set) false)
    ((= x (entry set)) #t)
    ((< x (entry set))
     (element-of-Tset? x (left-branch set)))
    ((> x (entry set))
     (element-of-Tset? x (right-branch set)))))

(define (adjoin-Tset x set)
  (cond
    ((null? set) (make-tree x nil nil))
    ((= x (entry)) set)
    ((< x (entry))
      (make-tree (entry x)
                 (adjoin-Tset x (left-branch set))
                 (right-branch set)))
    ((> x (entry))
      (make-tree (entry x)
                 (left-branch set)
                 (adjoin-Tset x (right-branch set))))))


(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
      (cons (entry tree)
        (tree->list-1
          (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                              (right-branch tree)
                              result-list))
                      )
      ))
  (copy-to-list tree '()))

(define (e-tree x)
  (make-tree x nil nil))

(define set1
  (make-tree 3 (e-tree 1) (make-tree 7 (e-tree 5) (make-tree 9 nil (e-tree 11)))))

