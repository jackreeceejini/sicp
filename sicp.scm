; Squre root approximation using newtons method
(define (sqrt x)
    (define (sqrt-iter guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))))
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
        (average guess (/ x guess)))
     (sqrt-iter 1.0))

(define (average x y)
    (/ (+ x y) 2))
(define (square x)
    (* x x))

;(print (sqrt 20))

; Tree recursion 
(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))

;(print (fib 7))

(define (fib-iter a b n)
    (if (< n 0)
        b
        (fib-iter (+ a b)
                  a
                  (- n 1))))
;(print (fib-iter 0 1 7))

;number of ways to make change
(define (count-change amount)
    (cc amount 5))
(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount 
                    (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                    kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 25)
            ((= kinds-of-coins 5) 50)))

;(print (count-change 100))

;compute sine
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle) 
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0)))))

;(print (sine 12.15))

; fast exponentiation
(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))
(define (even? n) 
    (= (remainder n 2) 0))

;(print (fast-expt 2 8))

;GCD
(define (GCD a b)
    (if (= b 0) 
        a
        (GCD b (remainder a b))))
;(print (GCD 206 40))

;;; Testing for primality

; Searching for divisors
(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))

;(print (prime? 2))

;Fermat test 

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
            (remainder (square (expmod base (/ exp 2) m))
                m))
            (else 
                (remainder (* base (expmod base (- exp 1) m))
                                m))))

; small pseudo random generator since no random in chicken scheme
(define *random-seed* 42) ; Set an initial seed

(define (random n)
  (set! *random-seed* (+ (* *random-seed* 1664525 1013904223) 12345))
  (modulo *random-seed* n))


(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))

; (print (smallest-divisor 199))
; (print (smallest-divisor 1999))
; (print (smallest-divisor 19999))

; (print (fast-prime? 41 30))

; 1.3 Formulating Abstractions with Higher-Order procedures

(define (cube x) (* x x x))

; 1.3.1 Procedures as Arguments 

; (define (sum-integers a b)
;     (if (> a b)
;         0
;         (+ a (sum-integers (+ a 1) b))))

; (define (sum-cubes a b)
;     (if (> a b)
;         0
;         (+ (cube a) (sum-cubes (+ a 1) b))))

; (define (pi-sum a b)
;     (if (> a b)
;         0
;         (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4)))))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
            (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
    (sum cube a inc b))

; (print (sum-cubes 1 10))

(define (identity x) x)
(define (sum-integers a b)
    (sum identity a inc b))

; (print (sum-integers 1 10))

(define (pi-sum a b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))

; (print (* 8 (pi-sum 1 1000)))

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
        dx))

; (print (integral cube 0 1 0.01))

; 1.3.3 Procedures as general methods

; Finding roots of equations by the half interval method

(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value) (search f neg-point midpoint))
                      ((negative? test-value) (search f midpoint pos-point))
                      (else midpoint))))))

(define (close-enough? x y)
    (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b)))
        (cond ((and (negative? a-value) (positive? b-value))
                (search f a b))
              ((and (negative? b-value) (positive? a-value))
                (search f b a))
            (else 
                (error "Values are not of opposite sign"  a b)))))

; (print (half-interval-method sin 2.0 4.0))

; Finding fixed points of functions

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(print (fixed-point cos 1.0))