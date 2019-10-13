"peoblem1a"
(define (is-prefix? a b)
  (if (equal? a '())
      #t
      (if (null? b)
          #f
          (if (= (car a) (car b))
              (is-prefix? (cdr a) (cdr b))
              #f))))
"tests1a"
"(is-prefix? '() '(1 2 3))"
(is-prefix? '() '(1 2 3))
"(is-prefix? '(3) '(1 2 3))"
(is-prefix? '(3) '(1 2 3))

"problems1b"
(define (common-prefix a b)
  (define (common-prefix-2 a b sofar)
    (if (or (null? a) (null? b))
        sofar
        (if (equal? (car a) (car b))
            (common-prefix-2 (cdr a) (cdr b) (append sofar (list (car a))))
            sofar)))
  (common-prefix-2 a b '()))
"tests1b"
"(common-prefix '(3 4) '(3 4 5))"
(common-prefix '(3 4) '(3 4 5))
"(common-prefix '(5 4) '(3 4 5))"
(common-prefix '(5 4) '(3 4 5))

"problem2a"
(define (gen-consecutive f a b)
  (if (> a b)
      '()
      (cons (f a) (gen-consecutive f (+ a 1) b))))
"tests2a"
"(gen-consecutive (lambda (x) (* x x)) 1 5)"
(gen-consecutive (lambda (x) (* x x)) 1 5)
"(gen-consecutive (lambda (x) (+ x 5)) 1 5)"
(gen-consecutive (lambda (x) (+ x 5)) 1 5)

"problem2b"
(define (gen-sequence f a b next)
  (if (> a b)
      '()
      (cons (f a) (gen-sequence f (next a) b next))))

"test2b"
"(gen-sequence (lambda (x) (+ x x)) 12 15 (lambda (x) (+ x 2)))"
(gen-sequence (lambda (x) (+ x x)) 12 15 (lambda (x) (+ x 2)))
"(gen-sequence (lambda (x) (/ x 2)) 10 20 (lambda (x) (+ x 2)))"
(gen-sequence (lambda (x) (/ x 2)) 10 20 (lambda (x) (+ x 2)))

"problem3"
(define (filter f 1st)
  (define (filter-help f 1st tempo)
    (if (null? 1st)
        tempo
        (if (f (car 1st))
            (filter-help f (cdr 1st) (append tempo (list (car 1st))))
            (filter-help f (cdr 1st) tempo))))
  (filter-help f 1st '()))
"tests3"
"(filter even? '(1 2 3 4 5 6 7))"
(filter even? '(1 2 3 4 5 6 7))
"(filter (lambda (x) (< (length x) 3))  '((1) (2 3) (4 5 6 7)))"
(filter (lambda (x) (< (length x) 3))  '((1) (2 3) (4 5 6 7)))

"problem4a"
(define (zip k)
  (define (zip-helper k1 k2)
    (if (null? k1)
        '()
        (cons (cons (car k1) (car k2)) (zip-helper (cdr k1) (cdr k2)))))
  (zip-helper (car k) (car (cdr k)) ))
"tests4a"
"(zip '((1 2 3 4 5) (5 4 3 2 1)))"
(zip '((1 2 3 4 5) (5 4 3 2 1)))
"(zip '((1 2 3 4 5) (1 2 3 4 5)))"
(zip '((1 2 3 4 5) (1 2 3 4 5)))

"problem4b"
(define (unzip k)
  (define (unzip-helper k temp1 temp2)
      (if (null? k)
        (cons temp1 (cons temp2 '()))
        (unzip-helper (cdr k)
                      (append temp1 (list (car (car k))))
                      (append temp2 (cdr (car k))))))
  (unzip-helper k '() '()))
"tests4b"
"(unzip '((1 2) (1 2) (1 2)))"
(unzip '((1 2) (1 2) (1 2)))

"problem5a"
(define (make-complex a b) (cons a b))
(define (real x)(car x)) (define (imag x)(cdr x))
(define (complex-add x y)
  (make-complex (+ (real x)(real y))(+ (imag x)(imag y))))
(define (complex-sub x y)
  (make-complex (- (real x)(real y))(- (imag x)(imag y))))
(define (complex-mult x y)
  (make-complex (- (* (real x)(real y))(* (imag x)(imag y)))
                (+ (* (real x)(imag y))(* (imag x)(real y)))))
(define (make-complex a b)
  (cons a b))
(define (sgn x)
  (cond ((= x 0) 0)
        ((< x 0) -1)
        ((> x 0) 1)))
(define (complex-sqrt x)
  (let ((a (car x))
        (b (car (cdr x))))
    (cons (sqrt (/ (+ a (sqrt (+ (* a a) (* b b)))) 2))
          (* (sgn b) (sqrt (/ (+ (- 0 a) (sqrt (+ (* a a) (* b b)))) 2))))))
"test5a"
"(complex-sqrt '(1 2))"
(complex-sqrt '(1 2))
(complex-mult (complex-sqrt '(1 2)) (complex-sqrt '(1 2)))
"(complex-sqrt '(2 0))"
(complex-sqrt '(2 0))
(complex-mult (complex-sqrt '(2 0)) (complex-sqrt '(2 0)))

"problem5b"
"when the real part is negative and the imaginary part is 0,"
"the square root of the fuction evaluates to 0"
"(complex-sqrt '(-2 0))"
(complex-sqrt '(-2 0))
(complex-mult (complex-sqrt '(-2 0)) (complex-sqrt '(-2 0)))
"(complex-sqrt '(20 0))"
(complex-sqrt '(20 0))
(complex-mult (complex-sqrt '(20 0)) (complex-sqrt '(20 0)))
"(complex-sqrt '(-20 0))"
(complex-sqrt '(-20 0))
(complex-mult (complex-sqrt '(-20 0)) (complex-sqrt '(-20 0)))

"problem5c"
"change to when x=0, (sgn x) evaluates to 1"
(define (sgn-1 x)
  (cond ((= x 0) 1)
        ((< x 0) -1)
        ((> x 0) 1)))
(define (complex-sqrt-2 x)
  (let ((a (car x))
        (b (car (cdr x))))
    (cons (sqrt (/ (+ a (sqrt (+ (* a a) (* b b)))) 2))
          (* (sgn-1 b) (sqrt (/ (+ (- 0 a) (sqrt (+ (* a a) (* b b)))) 2))))))
"(complex-sqrt-2 '(-2 0))"
(complex-sqrt-2 '(-2 0))
(complex-mult (complex-sqrt-2 '(-2 0)) (complex-sqrt-2 '(-2 0)))
"(complex-sqrt-2 '(20 0))"
(complex-sqrt-2 '(20 0))
(complex-mult (complex-sqrt-2 '(20 0)) (complex-sqrt-2 '(20 0)))
"(complex-sqrt-2 '(-20 0))"
(complex-sqrt-2 '(-20 0))
(complex-mult (complex-sqrt-2 '(-20 0)) (complex-sqrt-2 '(-20 0)))
