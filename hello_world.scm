(define (fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))

(print (+ 4 5))
(print (fact 12))