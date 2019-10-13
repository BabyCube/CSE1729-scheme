#lang racket
"problem-4a"
(define (throw-one-dice)
  (+ 1 (random 6)))
"problem-4b"
(define (dice-throw)
  (+ (throw-one-dice) (throw-one-dice)))
"problem-4c"
(define (make-point point)
  (let ((n (dice-throw)))
    (cond ((= n 7) #f)
          ((= n point) #t)
          (else (make-point n))))) 
(define (make-pass)
  (let ((m (dice-throw)))
    (cond ((= (dice-throw) (or 7 11)) #t)
          ((= (dice-throw) (or 2 3 12)) #f)
          (else (make-point m)))))
"problem-4d"
(define (win-odds n)
  (define (win-time n)
    (if (= n 0)
        0
        (if (make-pass)
            (+ 1 (win-time (- n 1)))
            (win-time (- n 1)))))
    (/ (win-time n) n))