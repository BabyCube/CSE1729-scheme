"peoblem1"
(define (function-merge f s-lst1 s-lst2)
  (cond ((null? s-lst1) s-lst2)
        ((null? s-lst2) s-lst1)
        ((f (car s-lst1) (car s-lst2))
         (cons (car s-lst1) (function-merge f
                                            (cdr s-lst1)
                                            s-lst2)))
        (else
         (cons (car s-lst2) (function-merge f
                                            s-lst1
                                            (cdr s-lst2))))))
(define (split l)
  (define (split-helper l l-1 l-2)
    (cond ((null? l) (cons l-1 (list l-2)))
          ((null? (cdr l)) (cons (append l l-1) (list l-2)))
          (else
           (split-helper (cddr l)
                         (cons (car l) l-1)
                         (cons (car (cdr l)) l-2)))))
  (split-helper l '() '()))
(define (mergesort l)
  (if (or (null? l) (null? (cdr l)))
      l
      (merge (mergesort (car (split l)))
             (mergesort (car (cdr (split l)))))))
(define (gen-mergesort f l)
  (if (or (null? l) (null? (cdr l)))
      l
      (function-merge f (gen-mergesort f (car (split l)))
                      (gen-mergesort f (car (cdr (split l)))))))

"tests1"
"(gen-mergesort > '(1 4 2 7))"
(gen-mergesort > '(1 4 2 7))
"(gen-mergesort = '(1 4 2 7))"
(gen-mergesort = '(1 4 2 7))
"(gen-mergesort < '(1 4 2 7))"
(gen-mergesort < '(1 4 2 7))

"problem2a"
(define (symbol<? x y)
  (string<? (symbol->string x) (symbol->string y)))
(define (symbol>? x y)
  (string>? (symbol->string x) (symbol->string y)))
(define (symbol=? x y)
  (string=? (symbol->string x) (symbol->string y)))
"tests2a"
"(symbol<? 'axiom 'boat)"
(symbol<? 'axiom 'boat)
"(symbol=? 'axiom 'boat)"
(symbol=? 'axiom 'boat)
"(symbol>? 'axiom 'boat)"
(symbol>? 'axiom 'boat)

"problem2b"
"when two strings are compared, it retures #t to string=? if
they have the same length and the same characters in order;
 use symbol=? has the same function as using eq? here, which
evaluates to #t if the same symbol"

"peoblem2c"
"(gen-mergesort symbol<? '(ad sd kf))"
(gen-mergesort symbol<? '(ad sd kf))
"(gen-mergesort symbol>? '(ad sd kf))"
(gen-mergesort symbol>? '(ad sd kf))

"problem3a"
(define (make-bst value left right)
  (list value left right))
(define (bst-value l)
  (car l))
(define (bst-left l)
  (cadr l))
(define (bst-right l)
  (caddr l))

"problem3b"
(define (bst-element? item bs-tree)
  (if (null? bs-tree)
      #f
      (cond ((equal? item (bst-value bs-tree)) #t)
            ((< item (bst-value bs-tree))
             (bst-element? item (bst-left bs-tree)))
            ((> item (bst-value bs-tree))
             (bst-element? item (bst-right bs-tree))))))
"tests3b"
"(bst-element? 5 '(3 (2 (1 () ())
                       (2.5 () ()))
                    (4 (3.7 () ())
                       (4.1 () ()))))"
(bst-element? 5 '(3 (2 (1 () ())
                       (2.5 () ()))
                    (4 (3.7 () ())
                       (4.1 () ()))))

"problem3c"
(define (bst-insert item bs-tree)
  (if (bst-element? item bs-tree)
      bs-tree
      (cond ((null? bs-tree)
             (make-bst item '() '()))
            ((< item (bst-value bs-tree))
             (make-bst
              (bst-value bs-tree)
              (bst-insert item (bst-left bs-tree))
              (bst-right bs-tree)))
            ((> item (bst-value bs-tree))
             (make-bst
              (bst-value bs-tree)
              (bst-left bs-tree)
              (bst-insert item (bst-right bs-tree)))))))
"tests3c"
"(bst-insert 5 '(3 (2 (1 () ())
                       (2.5 () ()))
                    (4 (3.7 () ())
                       (4.1 () ()))))"
(bst-insert 5 '(3 (2 (1 () ())
                     (2.5 () ()))
                  (4 (3.7 () ())
                     (4.1 () ()))))

"problem3d"
(define (bst-smallest bs-tree)
  (if (null? bs-tree)
      '(undefined)
      (if (null? (bst-left bs-tree))
          (bst-value bs-tree)
          (bst-smallest (bst-left bs-tree)))))
"tests3d"
"(bst-smallest '(3 (2 (1 () ())
                     (2.5 () ()))
                  (4 (3.7 () ())
                     (4.1 () ()))))"
(bst-smallest '(3 (2 (1 () ())
                     (2.5 () ()))
                  (4 (3.7 () ())
                     (4.1 () ()))))
"(bst-smallest '())"
(bst-smallest '())

"problem3e"
(define (bst-largest bs-tree)
  (if (null? bs-tree)
      '(undefined)
      (if (null? (bst-right bs-tree))
          (bst-value bs-tree)
          (bst-largest (bst-right bs-tree)))))
"tests3e"
"(bst-largest '(3 (2 (1 () ())
                     (2.5 () ()))
                  (4 (3.7 () ())
                     (4.1 () ()))))"
(bst-largest '(3 (2 (1 () ())
                    (2.5 () ()))
                 (4 (3.7 () ())
                    (4.1 () ()))))

"peoblem3f"
(define (bst-equal? bs-tree-1 bs-tree-2)
  (if (and (null? bs-tree-1)
           (null? bs-tree-2))
      #t
      (if (equal? (bst-value bs-tree-1)
                  (bst-value bs-tree-2))
          (if (and (bst-equal? (bst-left bs-tree-1)
                               (bst-left bs-tree-2))
                   (bst-equal? (bst-right bs-tree-1)
                               (bst-right bs-tree-2)))
              #t
              #f)
          #f)))
"tests3f"
"(bst-equal? '(3 (2 (1 () ())
                   (2.5 () ()))
                (4 (3.7 () ())
                   (4.1 () ())))
            '(3 (2 (1 () ())
                   (2.5 () ()))
                (4 (3.7 () ())
                   (4.1 () ()))))"
(bst-equal? '(3 (2 (1 () ())
                   (2.5 () ()))
                (4 (3.7 () ())
                   (4.1 () ())))
            '(3 (2 (1 () ())
                   (2.5 () ()))
                (4 (3.7 () ())
                   (4.1 () ()))))
"(bst-equal? '(3 (2 (1 () ())
                   (2.65 () ()))
                (4 (3.7 () ())
                   (4.1 () ())))
            '(3 (2 (1 () ())
                   (2.5 () ()))
                (4 (3.7 () ())
                   (4.1 () ()))))"
(bst-equal? '(3 (2 (1 () ())
                   (2.65 () ()))
                (4 (3.7 () ())
                   (4.1 () ())))
            '(3 (2 (1 () ())
                   (2.5 () ()))
                (4 (3.7 () ())
                   (4.1 () ()))))


"problem3g"
(define (bst-subset? bst1 bst2)
  (if (bst-element? (bst-value bst1) bst2)
      (cond ((and (null? (bst-left bst1))
                  (null? (bst-right bst1)))
             #t)
            ((null? (bst-right bst1))
             (bst-subset? (bst-left bst1) bst2))
            ((null? (bst-left bst1))
             (bst-subset? (bst-right bst1) bst2))
            (else
             (if (and (bst-subset? (bst-left bst1) bst2)
                      (bst-subset? (bst-right bst1) bst2))
                 #t
                 #f)))
      #f))
"tests3g"
"(bst-subset? '(1 () ())
            '(3 (2 (1 () ())
                   (2.5 () ()))
                (4 (3.7 () ())
                   (4.1 () ()))))"
(bst-subset? '(1 () ())
             '(3 (2 (1 () ())
                    (2.5 () ()))
                 (4 (3.7 () ())
                    (4.1 () ()))))


"problem3h"
(define (bst-set-equal? bst1 bst2)
  (if (and (bst-subset? bst1 bst2)
           (bst-subset? bst2 bst1))
      #t
      #f))
"tests3h"
"(bst-subset-equal? '(1 () ())
            '(3 (2 (1 () ())
                   (2.5 () ()))
                (4 (3.7 () ())
                   (4.1 () ()))))"
(bst-set-equal? '(1 () ())
                '(3 (2 (1 () ())
                       (2.5 () ()))
                    (4 (3.7 () ())
                       (4.1 () ()))))


"problem4a"
(define (bst-delete-min lst)
  (if (null? (bst-left lst))
      '()
      (make-bst (bst-value lst)
                (bst-delete-min (bst-left lst))
                (bst-right lst))))
"tests4a"
(bst-delete-min '(3 (2 (1 () ())
                       (2.5 () ()))
                    (4 (3.7 () ())
                       (4.1 () ()))))

"problem4b"
(define (bst-delete-max lst)
  (if (null? (bst-right lst))
      '()
      (make-bst (bst-value lst)
                (bst-left lst)
                (bst-delete-max (bst-right lst)))))
"tests4b"
(bst-delete-max '(3 (2 (1 () ())
                       (2.5 () ()))
                    (4 (3.7 () ())
                       (4.1 () ()))))

"problem4c"
(define (bst-insert-2 item bs-tree)
  (cond ((null? bs-tree)
         item)
        ((< (car item) (bst-value bs-tree))
         (make-bst
          (bst-value bs-tree)
          (bst-insert-2 item (bst-left bs-tree))
          (bst-right bs-tree)))
        ((> (car item) (bst-value bs-tree))
         (make-bst
          (bst-value bs-tree)
          (bst-left bs-tree)
          (bst-insert-2 item (bst-right bs-tree))))))
(define (bst-delete val bst)
  (if (bst-element? val bst)
      (cond ((equal? val (bst-value bst))
             (bst-insert-2 (bst-right bst)
                           (bst-left bst)))
            ((< val (bst-value bst))
             (make-bst (bst-value bst)
                       (bst-delete val (bst-left bst))
                       (bst-right bst)))
            ((> val (bst-value bst))
             (make-bst (bst-value bst)
                       (bst-left bst)
                       (bst-delete val (bst-right bst)))))
      bst))
"tests4c"
(bst-delete 5 '(10 (5 (4 () ())
                      (8 (7 () ())
                         (9 () ())))
                   (11 () ())))