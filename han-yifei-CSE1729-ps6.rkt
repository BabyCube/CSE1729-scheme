"problem1"
(define (my-equal? a b)
  (cond ((number? a)
         (if (number? b)
             (if (= a b) #t #f)
             #f))
        ((list? a)
         (if (list? b)       
             (cond
               ((null? a) (if (null? b) #t #f))
               ((null? b) (if (null? a) #t #f))
               ((equal? a b) #t)
               ((not (equal? (car a)
                             (car b))) #f)
               (else (my-equal? (cdr a) (cdr b))))
             #f))
        (else (if (eq? a b) #t #f))))
"tests1"
"(my-equal? '(a b c (1 2) 3.0) '(a b c (1 2) 3.0))"
(my-equal? '(a b c (1 2) 3.0) '(a b c (1 2) 3.0))
"(my-equal? '(a b c (1 2) 3.0) '(a b c (1 2)))"
(my-equal? '(a b c (1 2) 3.0) '(a b c (1 2)))
"(my-equal? 3 3.0)"
(my-equal? 3 3.0)
"(my-equal? 'a-1 'a-2)"
(my-equal? 'a-1 'a-2)

"problem2"
(define (nested-remove a b)
  (if (null? b)
      '()
      (if (list? a)
          (if (equal? a (car b))
              (nested-remove a (cdr b))
              (cons (car b) (nested-remove a (cdr b))))
          (if (list? (car b))
              (cons (nested-remove a (car b))
                    (nested-remove a (cdr b)))
              (if (equal? a (car b))
                  (nested-remove a (cdr b))
                  (cons (car b) (nested-remove a (cdr b))))))))
"tests2"
"(nested-remove '(a b) '(1 2 (a b)))"
(nested-remove '(a b) '(1 2 (a b)))
"(nested-remove 'b '(1 2 (a b)))"
(nested-remove 'b '(1 2 (a b)))

"problem3a"
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (subset? a b)
  (if (null? b)
      #t
      (if (element-of-set? (car b) a)
          (subset? a (cdr b))
          #f)))
"tests3a"
"(subset? '(a b c) '(a d))"
(subset? '(a b c) '(a d))
"(subset? '(1 2 3) '(1 2))"
(subset? '(1 2 3) '(1 2))

"problem3b"
(define (symmetric-set-difference a b)
  (if (null? b)
      '()
      (if (null? a)
          b
          (if (not (element-of-set? (car a) b))
              (cons (car a) (symmetric-set-difference (cdr a) b))
              (symmetric-set-difference (cdr a) (nested-remove (car a) b))))))
"tests3b"
"(symmetric-set-difference '(1 2 3 4 5) '(2 3 6 7 8))"
(symmetric-set-difference '(1 2 3 4 5) '(2 3 6 7 8))
"(symmetric-set-difference '(1 a 2 b 3 c) '(3 c 4 d 5 e))"
(symmetric-set-difference '(1 a 2 b 3 c) '(3 c 4 d 5 e))

"problem3c"
(define  (cartesian-product a b)
 (let ((k b))
  (define (cartesian-helper a b temp)
   (if (null? a)
      temp
     (if (null? b)
        (cartesian-helper (cdr a) k temp)
        (cartesian-helper a (cdr b) (cons (cons (car a) (car b)) temp)))))
   (cartesian-helper a b '())
(cartesian-helper a b '())))
"tests3c"
"(cartesian-product '(1 2 3) '(2 3 4))"
(cartesian-product '(1 2 3) '(2 3 4))

"problem4"
(define (denestify x)
  (if (null? x)
      '()
      (if (list? (car x))
          (append (denestify (car x)) (denestify (cdr x)))
          (cons (car x) (denestify (cdr x))))))
"tests4"
"(denestify '((a b) c (d e (f g))))"
(denestify '((a b) c (d e (f g))))
"(denestify '(1 2 3 4))"
(denestify '(1 2 3 4))
"(denestify '(1 2 (3 4)))"
(denestify '(1 2 (3 4)))

"problem5a"
(define (partition lst pivot)
  (define (partition-helper lst-2 pivot left right mid)
    (cond ((null? lst-2)
           (list left mid right))
          ((> (car lst-2) pivot)
           (partition-helper (cdr lst-2) pivot left (cons (car lst-2) right) mid))
          ((< (car lst-2) pivot)
           (partition-helper (cdr lst-2) pivot (cons (car lst-2) left) right mid))
          (else
           (partition-helper (cdr lst-2) pivot left right (cons (car lst-2) mid)))))
  (partition-helper lst pivot '() '()'()))
"tests5a"
"(partition '(1 3 4 2 5 9 11 3 5 8) 5)"
(partition '(1 3 4 2 5 9 11 3 5 8) 5)

"problem5b"
;(define (sort lst)
 ; (define (sort-helper lst left right mid)
  ;  (cond ((and (null? (cdr left))
   ;             (null? (cdr mid))
    ;            (null? (cdr right))) (append (left mid right)))
     ;     (else (partition lst (car lst))))))
          
  
;(define (k-smallest lst k)
;(let ((llst (sort lst)))
;(if (= k 0)
 ;   (car llst)
  ;  (k-smallest (cdr llst) (- k 1))))
    

"problem6"
(define (merge l-1 l-2)
  (define (merge-helper l-1 l-2 lst)
    (if (and (null? l-1) (null? l-2))
        lst
        (cond ((and (null? l-1) (not (null? l-2)))
               (merge-helper '() '() (append lst l-2)))
              ((and (null? l-2) (not (null? l-1)))
               (merge-helper '() '() (append lst l-1)))
              (else
               (cond ((< (car l-1) (car l-2))
                      (merge-helper
                       (cdr l-1)
                       l-2
                       (append lst (list (car l-1)))))
                     ((> (car l-1) (car l-2))
                      (merge-helper
                       l-1
                       (cdr l-2)
                       (append lst (list (car l-2)))))
                     ((= (car l-1) (car l-2))
                      (merge-helper
                       (cdr l-1)
                       (cdr l-2)
                       (append lst (list (car l-1))))))))))
  (merge-helper l-1 l-2 '()))
"tests6"
"(merge '(1 2 3 4 7 8 10 12) '(4 5 6 7 8 9))"
(merge '(1 2 3 4 7 8 10 12) '(4 5 6 7 8 9))