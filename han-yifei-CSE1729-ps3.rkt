"problem-1"
(define (fact n)
  (define (fact-f a b)
    (if (= a 0)
        b
        (fact-f (- a 1) (* a b)
                )))
  (fact-f n 1))
(define (new-sin x n)
  (if (or (> n 0) (= n 0))
      (if (= n 0)
          x
          (let ((k (+ 1 (* 2 n))))
            (+ (/ (* (expt -1 n)
                     (expt x k))
                  (fact k))
               (new-sin x (- n 1)))))
      #f))
"test-1"
"(new-sin 45 100)"
(new-sin 45 100)
"(new-sin 1 1000)"
(new-sin 1 1000)
"(new-sin 45 -100)"
(new-sin 45 -100)

"problem-2a"
(define (lucas n)
  (if (< n 0)
      #f
      (cond ((= n 0) 2)
            ((= n 1) 1)
            (else (+ (lucas (- n 1)) (lucas (- n 2)))))))
"test2a"
"(lucas 12)"
(lucas 12)
"(lucas 2)"
(lucas 2)
"(lucas 5)"
(lucas 5)

"problem-2b"
(define (lucas-ratio n)
  (if (> n 0)
      (/ (+ 0.0 (lucas n)) (lucas (- n 1)))
      #f))
(define (fib-1 flr a b)
  (if (= flr 0)
      b
      (fib-1 (- flr 1) (+ a b) a)))
(define (fib n)
  (cond ((< n 0) #f)
        ((= n 0) 0)
        ((= n 1) 1)
        (else
         (fib-1 n 1 0))))
(define (fibonacci-ratio n)
  (if (> n 0)
      (/ (+ 0.0 (fib n)) (fib (- n 1)))
      #f))
"l20 & f20"
(lucas-ratio 20)
(fibonacci-ratio 20)
"l21 & f21"
(lucas-ratio 21)
(fibonacci-ratio 21)
"l22 & f22"
(lucas-ratio 22)
(fibonacci-ratio 22)
"the result of the ratio might be the golden ratio"
"problem-2c"
"L30"
(lucas-ratio 30)
"L35"
(lucas-ratio 35)
"L40"
(lucas-ratio 40)
"It will not come up with an answer as the memory for calculation may be used up"
(define (fast-lucas k)
  (define (lucas-iter step a b)
    (if (= step 0)
        b
        (lucas-iter (- step 1) (+ a b) a)))
  (lucas-iter k 1 2))
"(fast-lucas 2)"
(fast-lucas 2)
"(fast-lucas 4)"
(fast-lucas 4)
"(fast-lucas 6)"
(fast-lucas 6)
"lucas check"
"lucas 3"
(fast-lucas 3)
(lucas 3)
"lucas 4"
(fast-lucas 4)
(lucas 4)
"lucas 5"
(fast-lucas 5)
(lucas 5)
"(fast-lucas 50)"
(fast-lucas 50)
"(fast-lucas 50000)"
(fast-lucas 50000)
"k=3:4, 3
 k=4:8, 4
 k=5:14,5
 k=6:24,6"
"problem-3a"
(define (golden-ratio-approximate n)
  (if (= n 1)
      2
      (+ 1 (/ 1 (golden-ratio-approximate (- n 1))))))
"test-3a"
"(golden-ratio-approximate 1)"
(golden-ratio-approximate 1)
"(golden-ratio-approximate 3)"
(golden-ratio-approximate 3)
"(golden-ratio-approximate 5)"
(golden-ratio-approximate 5)
"problem-3b"
(define (arge x y)
  (/ (+ x y) 2))
(define (square c) (* c c))
(define (golden-approx a b tol)
  (if (< (abs (- a b)) tol)
      a
      (if (< (square (arge a b)) (+ 1 (arge a b)))
          (golden-approx (arge a b) b tol)
          (golden-approx a (arge a b) tol))))
"test-3b"
"(golden-approx 1 2 0.0001)"
(golden-approx 1 2 0.0001)
"(golden-approx -1 12 0.0005)"
(golden-approx -1 12 0.0005)
"(golden-approx 1.2 1.9 0.0015)"
(golden-approx 1.2 1.9 0.0015)
"problem-3c"
"the second one is better because we can obtain the more accurate result as we want"