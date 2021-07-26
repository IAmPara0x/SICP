
; Implementation of huffman encoding trees

(define nil '())

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define symbol-leaf cadr )
(define weight-leaf caddr)

(define (make-code-tree left right)
  (list
     left
     right
     (append (symbols left) (symbols right))
     (+ (weight left) (weight right))))

(define left-branch car)
(define right-branch cadr)

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (list (weight-leaf tree))
    (caddr tree)))

(define (decode bits tree)
  (define (decoce-1 bits current-branch)
    (if (null? bits)
      nil
      (let (
            (next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decoce-1 (cdr bits) tree))
          (symbol-leaf (cdr bits) next-branch)))))
  (decoce-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit"))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;TODO: complete huffman encoding
