> "problem-1"

  (define (number-sum n)
    (if (= n 0)
        0
        (+ n (number-sum (- n 1)))))
"problem-1a"
(define (odd-sum n)
   (if (= n 1)
       1
       (/ (- (number-sum (* 2 n)) n) 2)))
"test1a"
"(odd-sum 2)"
(odd-sum 2)
"(odd-sum 3)"
(odd-sum 3)
"(odd-sum 4)"
(odd-sum 4)
"problem-1b"
"(odd-sum 1)"
(odd-sum 1)
"(odd-sum 2)"
(odd-sum 2)
"(odd-sum 3)"
(odd-sum 3)
"(odd-sum 4)"
(odd-sum 4)
"(odd-sum 5)"
(odd-sum 5)
"(odd-sum 6)"
(odd-sum 6)
"(odd-sum 7)"
(odd-sum 7)
"the result of odd-sum is the square of the input number"
"problem-1c"
(define (sum-from-to a b)
  (if (> a b)
      0
      (- (number-sum b) (number-sum (- a 1)))))
"test1c"
"(sum-from-to 3 5)"
(sum-from-to 3 5)
"(sum-from-to 10 5)"
(sum-from-to 10 5)
"(sum-from-to 8 10)"
(sum-from-to 8 10)
"problem-2"
(define (q2-element n)
  (- 1
     (/ 1
        n)))
(define (result-of-q2 k)
  (if (= k 2)
      1/2
      (* (q2-element k) (result-of-q2 (- k 1)))))
"test2"
"(result-of-q2 3)"
(result-of-q2 3)
"(result-of-q2 4)"
(result-of-q2 4)
"(result-of-q2 5)"
(result-of-q2 5)
"problem-3a"
(define (finite-sum-of-powers z k)
  (cond
    ((or (= z 0) (< z 0) (> z 1)) #f)
    (else
  (if (= k 1)
      z
  (+ (expt z k) (finite-sum-of-powers z (- k 1)))))))
"test3a"
"(finite-sum-of-powers 10 2)"
(finite-sum-of-powers 10 2)
"(finite-sum-of-powers 0.1 4)"
(finite-sum-of-powers 0.1 4)
"(finite-sum-of-powers 0 2)"
(finite-sum-of-powers 0 2)
"problem-3b"
(define k 1)
(define (value z) (/ z (- 1 z)))
(define (first-value-k-or-higher z tol k)
   (cond
    ((or (= z 0) (< z 0) (> z 1)) #f)
    (else
  (if (< (abs (- (finite-sum-of-powers z k) (value z))) tol)
      k
      (first-value-k-or-higher z tol (+ k 1))))))
(define (terms-needed z tol) (first-value-k-or-higher z tol k))
"tests3b"
"(terms-needed 0.2 0.000000001)"
(terms-needed 0.2 0.000000001)
"(terms-needed 0.01 0.000000001)"
(terms-needed 0.01 0.000000001)
"(terms-needed 0.99 0.000000001)"
(terms-needed 0.99 0.000000001)
"problem-3c"
"the smaller z reqires a smaller numbers of terms needed"
"problem-4a"
(define (sum-of-k k)
  (if (= k 1)
      4.0
  (+
   (* (/ 4 (- (* 2 k) 1)) (expt -1 (+ 1 k)))
   (sum-of-k (- k 1)))))
"test-4a"
"(sum-of-k 3)"
(sum-of-k 3)
"(sum-of-k 1)"
(sum-of-k 1)
"(sum-of-k 4)"
(sum-of-k 4)
"problem-4b"
(sum-of-k 100)
"problem-4c"
(sum-of-k 100000)
"it is the approximate number of PI"
"problem-4d"
"100,000-1 calls were made to fuction expt;
the actual value of the last call is 3"
"problem-4e"
(define (sum-of-k-1 k)
  (if (= k 1)
      4.0
  (if (= (modulo k 2) 0)
      (+
   (* (/ 4 (- (* 2 k) 1)) -1)
   (sum-of-k-1 (- k 1)))
      (+
       (* (/ 4 (- (* 2 k) 1)))
   (sum-of-k-1 (- k 1))))))
"test-4e"
"(sum-of-k1 100000)"
(sum-of-k-1 100000)
"(sum-of-k-1 2)"
(sum-of-k-1 2)
"(sum-of-k-1 3)"
(sum-of-k-1 3)
      
       