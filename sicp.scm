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
(print (GCD 206 40))

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



