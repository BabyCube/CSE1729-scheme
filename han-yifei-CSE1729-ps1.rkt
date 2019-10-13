
;(22 + 42)*(54*99)
(* (+ 22 42) (* 54 99))


;((22 + 42)*54)*99
(* (* (+ 22 42) 54) 99)

;64*102+16*(44/22)
(+ (* 64 102) (* 16 (/ 44 22)))

; cube, the function cube(x) = x3
(define (cube x) (* x x x))

;p, the polynomial function p(x) = (x5 + 11x4 + 24x3 - x + 39)2.
(define (p-p x) (+
                 (* x x x x x)
                  (* 11 (* x x x x))
                   (* 24 (* x x x))
                   (- 0 x)
                   39))
(define (p x) (* (p-p x) (p-p x)))

;the function eighth(x) = x8
(define (eighth x) (* (cube x) (cube x) x x))

; sixty-fourth(x) = x64
(define (sixty-fourth x) (* (eighth x) (eighth x) (eighth x) (eighth x) (eighth x) (eighth x) (eighth x) (eighth x)))

;(is-triangle? a b c)
(define (is-triangle? a b c) (and (> (+ a b) c) (> (+ b c) a) (> (+ a c) b)))

;(area a b c)
(define (s-value a b c) (/ (+ a b c) 2))
(define (area a b c) (sqrt (* (s-value a b c) (- (s-value a b c) a) (- (s-value a b c) b) (- (s-value a b c) c))))

                   

