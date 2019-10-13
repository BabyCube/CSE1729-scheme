(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (stream-car x)
  (car x))

(define (stream-cdr x)
  (force (cdr x)))

(define stream-null? null?)

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (str-to-lst str k)
  (if (equal? k 1)
      (list (stream-car str))
      (cons (stream-car str)
            (str-to-lst (stream-cdr str) (- k 1)))))

(define (factorial x)
  (define (factorial-helper x counter)
    (if (equal? x 1)
        counter
        (factorial-helper (- x 1) (* x counter))))
  (factorial-helper x 1))


(define (power base exp)
  (cond ((= exp 0) 1)
        (#t (* base (power base (- exp 1))))))

(define (partial-sums str)
  (define (partial-sums-help str counter)
    (cons-stream (+ (stream-car str) counter)
                 (partial-sums-help (stream-cdr str)
                                    (+ (stream-car str) counter))))
  (partial-sums-help str 0))

(define (mult-streams str1 str2)
  (cond ((and (stream-null? str1) (stream-null? str2)) '())
        ((stream-null? str1) '())
        ((stream-null? str2) '())
        (else (cons-stream (* (stream-car str1) (stream-car str2))
                           (mult-streams (stream-cdr str1)
                                         (stream-cdr str2))))))

(define (add-streams str1 str2)
  (cond ((and (stream-null? str1) (stream-null? str2)) '())
        ((stream-null? str1) '())
        ((stream-null? str2) '())
        (else (cons-stream (+ (stream-car str1) (stream-car str2))
                           (add-streams (stream-cdr str1)
                                        (stream-cdr str2))))))

"Queestion-1"
(define one (cons-stream 1 one))
(define ints (cons-stream 1 (add-streams one ints)))
(define factorial-stream
  (cons-stream 1 (mult-streams factorial-stream
                               ints)))
"Test-1"
(str-to-lst factorial-stream 5)

"Question-2"
(define neg-power-2
  (cons-stream (/ 1 2) (scale-stream neg-power-2 (/ 1 2))))
"Test-2"
"(str-to-lst neg-power-2 4)"
(str-to-lst neg-power-2 4)

"Question-3a"
(define (sin-stream x)
  (define (sin-helper x k)
    (cons-stream (/ (* (power -1 k)
                       (power x (+ (* 2 k) 1)))
                    (factorial (+ 1 (* 2 k))))
                 (sin-helper x (+ k 1))))
  (sin-helper x 0))
"Test-3a"
(sin-stream 1)
(str-to-lst (sin-stream 1) 10)
(sin-stream 2)
(str-to-lst (sin-stream 2) 10)

"Question-3b"
(define (sin-stream-added x)
  (partial-sums (sin-stream x)))
"Test-3b"
(sin-stream-added 5)
(str-to-lst (sin-stream-added 5) 10)

"Question-4a"
(define (stream-merge str1 str2)
  (cond ((and (stream-null? str1) (stream-null? str2)) '())
        ((stream-null? str1) str2)
        ((stream-null? str2) str1)
        (else
         (cond ((> (stream-car str1) (stream-car str2))
                (cons-stream (stream-car str2)
                             (stream-merge str1 (stream-cdr str2))))
               ((> (stream-car str2) (stream-car str1))
                (cons-stream (stream-car str1)
                             (stream-merge (stream-cdr str1) str2)))
               ((= (stream-car str1) (stream-car str2))
                (cons-stream (stream-car str1)
                             (stream-merge (stream-cdr str1) (stream-cdr str2))))))))
"Test-4a"
"(stream-merge ints (stream-cdr factorial-stream))"
(stream-merge ints (stream-cdr factorial-stream))
(str-to-lst (stream-merge ints (stream-cdr factorial-stream)) 10)

"Question-4b"
(define stream-235
  (stream-merge (stream-merge (cons-stream 1 (scale-stream stream-235 2))
                              (cons-stream 1 (scale-stream stream-235 3)))
                (cons-stream 1 (scale-stream stream-235 5))))
"Test-4b"
(str-to-lst stream-235 10)

"Question-5"
(define (infinite-helper f)
  (define (infinite-counter k)
    (cons-stream (f k) (infinite-counter (+ k 1)))
    )
  (infinite-counter 1))
(define (golden-stream)
  (define (golden-stream-helper k)
    (if (equal? k 1)
        2
        (+ 1 (/ 1 (golden-stream-helper (- k 1))))))
  (infinite-helper golden-stream-helper))
"Test-5"
"(golden-stream)"
(golden-stream)
"(str-to-lst (golden-stream) 10)"
(str-to-lst (golden-stream) 10)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define pairs-stream (pairs ints ints))

"Question-6a"
(define (merge-weighted stream-1 stream-2 weight-generator)
  (cond ((stream-null? stream-1) stream-2)
        ((stream-null? stream-2) stream-1)
        (else (cond ((< (weight-generator (stream-car stream-1))
                        (weight-generator (stream-car stream-2)))
                     (cons-stream (stream-car stream-1)
                                  (merge-weighted (stream-cdr stream-1) stream-2 weight-generator)))
                    ((> (weight-generator (stream-car stream-1))
                        (weight-generator (stream-car stream-2)))
                     (cons-stream (stream-car stream-2)
                                  (merge-weighted stream-1 (stream-cdr stream-2) weight-generator)))
                    ((= (weight-generator (stream-car stream-1))
                        (weight-generator (stream-car stream-2)))
                     (if (equal? (stream-car stream-1) (stream-car stream-2))
                         (cons-stream (stream-car stream-2)
                                  (merge-weighted (stream-cdr stream-1)
                                                  (stream-cdr stream-2) weight-generator))
                     (cons-stream (stream-car stream-1) (cons-stream (stream-car stream-2)
                                  (merge-weighted (stream-cdr stream-1)
                                                  (stream-cdr stream-2) weight-generator)))))))))
"Test-6a"
(define (weight-test x) (+ x x))
(define (odd-stream-from a) (cons-stream a (odd-stream-from (+ a 2))))
;test case for the odd-stream-from
;(odd-stream-from 3)
;(str-to-lst (odd-stream-from 3) 10)
(merge-weighted (odd-stream-from 3) ints weight-test)
(str-to-lst (merge-weighted (odd-stream-from 3) ints weight-test) 10)

"Question-6b"
(define (weighted-pairs stream-1 stream-2 weight-function)
  (cons-stream (list (stream-car stream-1) (stream-car stream-2))
               (merge-weighted (stream-map
                                (lambda (stream-element) (list (stream-car stream-1) stream-element))
                                (stream-cdr stream-2))
                               (weighted-pairs (stream-cdr stream-1) (stream-cdr stream-2) weight-function)
                               weight-function)))
(define (weight-test-6b pair) (+ (car pair) (cadr pair)))
                               
"Test-6b"
"(weighted-pairs ints ints weight-test-6b)"
(weighted-pairs ints ints weight-test-6b)
"(str-to-lst (weighted-pairs ints ints weight-test-6b) 10)"
(str-to-lst (weighted-pairs ints ints weight-test-6b) 10)

"Question-6c"
(define all-positive-integer (weighted-pairs ints ints weight-test-6b))
"Test-6c"
all-positive-integer
(str-to-lst all-positive-integer 10)

"Question-6d"
(define (weight-cube pair) (+ (* (car pair) (car pair) (car pair))
                              (* (cadr pair) (cadr pair) (cadr pair))))
(define cube-order-stream (weighted-pairs ints ints weight-cube))
(str-to-lst cube-order-stream 15)
(define (ramanujan-number-stream n)
  