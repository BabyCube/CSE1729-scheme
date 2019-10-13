"problem-1a"
(define (g-sum f x a b)
  (if (= a b)
      (f x a)
      (+ (f x b) (g-sum f x a (- b 1)))))
(define (factorial x)
  (fac-iter x 1))
(define (fac-iter x sofar)
  (if (= x 1)
      sofar
      (fac-iter (- x 1) (* x sofar)))) 
(define (element x n)
  (* (expt -1 n) (/ (expt x (+ (* 2 n) 1))
                    (factorial (+ 1 (* 2 n))))))
(define (new-sin x n)
  (g-sum element x 0 n))
"test1a"
"(new-sin 0 15)"
(new-sin 0 15)
"(new-sin 45 100)"
(new-sin 45 100)
"(new-sin 1 1000)"
(new-sin 1 1000)

"problem-1b"
(define (g-sum-i f a b)
  (let ((steps (- b a)))
    (define (g-sum-iter2 f a b sofar)
      (if (= a (+ b 1))
          sofar
          (g-sum-iter2 f a (- b 1) (+ sofar (f b)))))
    (g-sum-iter2 f a b 0)))
"tests"
"(g-sum-i (lambda (x) x) 0 3)"
(g-sum-i (lambda (x) x) 0 3)
"(g-sum-i (lambda (x) (* x x)) 0 3)"
(g-sum-i (lambda (x) (* x x)) 0 3)
"(g-sum-i (lambda (x) (* x x)) 1 3)"
(g-sum-i (lambda (x) (* x x)) 1 3)

"peoblem-1c"
"tests"
"(new-sin 1 1)"
(new-sin 1 1)
"(new-sin 1 50)"
(new-sin 1 50)
"(new-sin 1 150)"
(new-sin 1 150)
"(new-sin 2 100)"
(new-sin 2 100)
"(new-sin 2 200)"
(new-sin 2 200)
"(new-sin 3 150)"
(new-sin 3 150)
"(new-sin 3 50)"
(new-sin 3 50)

"problem-2a"
(define (g-product term a b)
  (if (= a b)
      (term a)
      (* (term b) (g-product term a (- b 1)))))
"tests"
"(g-product (lambda (x) x) 0 5)"
(g-product (lambda (x) x) 0 5)
"(g-product (lambda (x) x) 1 5)"
(g-product (lambda (x) x) 1 5)
"(g-product (lambda (x) 5) 0 2)"
(g-product (lambda (x) 5) 0 2)

"problem-2b"
(define (g-product-i term a b)
  (define (g-product-iter term a b sofar)
    (if (= a (+ b 1))
        sofar
        (g-product-iter term a (- b 1) (* (term b) sofar))))
  (g-product-iter term a b 1))
"tests"
"(g-product-i (lambda (x) x) 0 5)"
(g-product-i (lambda (x) x) 0 5)
"(g-product-i (lambda (x) x) 1 5)"
(g-product-i (lambda (x) x) 1 5)
"(g-product-i (lambda (x) 5) 0 2)"
(g-product-i (lambda (x) 5) 0 2)

"problem-2c"
(define (pi-approxi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))
(define (pi-approx n)
  (* 4.0 (g-product-i pi-approxi-term 1 n)))

"problem-2d"
"(pi-approx 1)"
(pi-approx 1)
"(pi-approx 100)"
(pi-approx 100)
"(pi-approx 1000)"
(pi-approx 1000)
"the larger n calls for a more precise approximation to the actual pi value"

"problem-3a"
"recursive"
(define (accumulate combiner null-value term a next b)
  (combiner null-value (accumulate-1 combiner null-value term a next b)))

(define (accumulate-1 combiner null-value term a next b)
  (if (= a b)
      (term a)
      (combiner (term b) (accumulate-1 combiner null-value term a next (next b)))))
"tests"
"(accumulate + 100 (lambda (x) x) 1 (lambda (x) (- x 1)) 2)"
(accumulate + 100 (lambda (x) x) 1 (lambda (x) (- x 1)) 2)
"(accumulate * 0 (lambda (x) x) 1 (lambda (x) (- x 1)) 2)"
(accumulate * 0 (lambda (x) x) 1 (lambda (x) (- x 1)) 2)
"(accumulate + 10 (lambda (x) (* x x)) 1 (lambda (x) (- x 1)) 5)"
(accumulate + 10 (lambda (x) (* x x)) 1 (lambda (x) (- x 1)) 5)
"iterative"
(define (accumulate-iter combiner null-value term a next b)
   (if (=  (combiner 4 5) 20)
  (combiner null-value (accumulate-2 combiner null-value term a next b 1))
  (combiner null-value (accumulate-2 combiner null-value term a next b 0))))

(define (accumulate-2 combiner null-value term a next b sofar)
  (if (= a (+ 1 b))
      sofar
      (accumulate-2 combiner null-value term a next (next b) (combiner (term b) sofar))))
"tests"
"(accumulate-iter + 100 (lambda (x) x) 1 (lambda (x) (- x 1)) 2)"
(accumulate-iter + 100 (lambda (x) x) 1 (lambda (x) (- x 1)) 2)
"(accumulate-iter * 0 (lambda (x) x) 1 (lambda (x) (- x 1)) 2)"
(accumulate-iter * 0 (lambda (x) x) 1 (lambda (x) (- x 1)) 2)
"(accumulate-iter + 10 (lambda (x) (* x x)) 1 (lambda (x) (- x 1)) 5)"
(accumulate-iter + 10 (lambda (x) (* x x)) 1 (lambda (x) (- x 1)) 5)

"problem-3b"
"g-sum-2"
(define (g-sum-2 term a b)
  (accumulate-iter + 0 term a (lambda (x) (- x 1)) b))
"g-product-2"
(define (g-product-2 term a b)
  (accumulate-iter * 1 term a (lambda (x) (- x 1)) b))
"tests"
"(g-sum-2 (lambda (x) x) 0 3)"
(g-sum-2 (lambda (x) x) 0 3)
"(g-sum-2 (lambda (x) (* x x)) 0 3)"
(g-sum-2 (lambda (x) (* x x)) 0 3)
"(g-sum-2 (lambda (x) (* x x)) 1 3)"
(g-sum-2 (lambda (x) (* x x)) 1 3)
"(g-product-2 (lambda (x) x) 0 5)"
(g-product-2 (lambda (x) x) 0 5)
"(g-product-2 (lambda (x) x) 1 5)"
(g-product-2 (lambda (x) x) 1 5)
"(g-product-2 (lambda (x) 5) 0 2)"
(g-product-2 (lambda (x) 5) 0 2)

"peoblem3c"

"problem4a"
(define (der function x h)
  (/ (- (function (+ x h)) (function x)) h))
"problem4b"
(define pie 3.14159265358979323846264338327950288419716939937510582)
"compare g & cos: 0"
(der (lambda (x) (sin x)) 0 0.5)
(cos 0)
"compare g & cos: 0.5pie"
(der (lambda (x) (sin x)) (/ pie 2) 0.5)
(cos (/ pie 2))
"compare g & cos: pie"
(der (lambda (x) (sin x)) pie 0.5)
(cos pie)
"compare g & cos: 0.75pie"
(der (lambda (x) (sin x)) (* 0.75 pie) 0.5)
(cos (* 0.75 pie))
"problem4c"
(define (fun x)
  (+ (* x x 3) (* -2 x) 7))
(define (actural-der x) (- (* 6 x) 2))
"der (fun 5) & 6x-2"
(der (lambda (x) (fun x)) 5 0.5)
(actural-der 5)
"der (fun 10) & 6x-2"
(der (lambda (x) (fun x)) 10 0.5)
(actural-der 10)
"der (fun 15) & 6x-2"
(der (lambda (x) (fun x)) 15 0.5)
(actural-der 15)
"der (fun 120) & 6x-2"
(der (lambda (x) (fun x)) 120 0.5)
(actural-der 120)

"problem-5a"
(define (comp f g) (lambda (x)(f (g x))))
(define (complement f)
  (comp not f))
"tests"
"((complement sin) 5)"
((complement sin) 5)

"problem 5b"
(define (fun-sum f g)
  (lambda (x) (+ (f x) (g x))))
(define (fun-product f g)
  (lambda (x) (* (f x) (g x))))
"tests"
"(fun-sum (lambda (x) 1) (lambda (x) 11) 1)"
((fun-sum (lambda (x) 1) (lambda (x) 11)) 2)
"((fun-sum sin cos) 0.0)"
((fun-sum sin cos) 0.0)

"problem 5c"
(define (der-product-rule f g)
  (lambda (x)
  (fun-sum (fun-product (der g x 0.001) (f x))
           (fun-product (der f x 0.001) (g x)))))
"tests"
"((der-product-rule (lambda (x) 3) (lambda (x) 10)) 5)"
((der-product-rule (lambda (x) 3) (lambda (x) 10)) 5)

"problem 5d"
(define (der-product-direct f g x)
  (der (fun-product f g) x 0.0001))

"problem 5e"
((der-product-rule
 (lambda(x) (* x x))
 (lambda(x) (* 2 x))) 2)
"why is this procedure??"
