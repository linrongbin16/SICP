(define (sicp)
  (print "sicp:"))

(define size 2)
(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y) (= x y)))

(define (boolean-to-string x)
  (if x
      "true"
      "false"))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (average a b)
    (/ (+ a b) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; (sicp)
; (display (format "abs -5:~d, 5:~d~%" (abs -5) (abs 5)))
; (display (format "abs2 -5:~d, 5:~d~%" (abs2 -5) (abs2 5)))
; (display (format "abs3 -5:~d, 5:~d~%" (abs3 -5) (abs3 5)))
; (display (format "0 <= 3 <= 3:~s~%" (boolean-to-string (and (>= 3 0) (>= 3 3)))))
; (display (format "square 5:~d~%" (square 5)))
; (display (format "sqrt 3: ~a^2 = ~a, sqrt 11: ~a^2 = ~a~%" (sqrt 3) (* (sqrt 3) (sqrt 3)) (sqrt 11) (* (sqrt 11) (sqrt 11))))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))

; (define (fib n)
;   (cond ((= n 0) 0)
;         ((= n 1) 1)
;         (else (+ (fib (- n 1))
;                  (fib (- n  2))))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; (sicp)
; (display (format "factorial 7 = ~a~%" (factorial 7)))
; (display (format "fibonacci 8 = ~a~%" (fib 8)))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; (sicp)
; (display (format "count-change 100 = ~a~%" (count-change 100)))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b (- counter 1) (* b product))))
  (expt-iter b n 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; (sicp)
; (display (format "expt 3 3 = ~a~%" (expt 3 3)))
; (display (format "expt 2 10 = ~a~%" (expt 2 10)))
; (display (format "fast-expt 3 3 = ~a~%" (fast-expt 3 3)))
; (display (format "fast-expt 2 10 = ~a~%" (fast-expt 2 10)))
; (display (format "even? 10 = ~a~%" (even? 10)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; (sicp)
; (display (format "remainder 3 3 = ~a~%" (remainder 3 3)))
; (display (format "gcd 3 3 = ~a~%" (gcd 3 3)))

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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; (sicp)
; (display (format "prime? 3 = ~a~%" (prime? 3)))
; (display (format "prime? 11 = ~a~%" (prime? 11)))
; (display (format "prime? 13148217 = ~a~%" (prime? 13148217)))
; (display (format "prime? 13148219 = ~a~%" (prime? 13148219)))
; (display (format "prime? 13148221 = ~a~%" (prime? 13148221)))

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; (sicp)
; (display (format "sum-cubes 3 5 = ~a~%" (sum-cubes 3 5)))
; (display (format "sum-cubes 1 10 = ~a~%" (sum-cubes 1 10)))
; (display (format "sum-integers 3 50 = ~a~%" (sum-integers 3 50)))
; (display (format "sum-integers 1 10 = ~a~%" (sum-integers 1 10)))
; (display (format "(* 8 (pi-sum 1 1000)) = ~a~%" (* 8 (pi-sum 1 1000))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; (sicp)
; (display (format "integral cube 0 1 0.01 = ~a~%" (integral cube 0 1 0.01)))
; (display (format "integral cube 0 1 0.001 = ~a~%" (integral cube 0 1 0.001)))

(define (average x y)
  (/ (+ x y) 2.0))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
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
           (error "Values are not of opposite sign" a b)))))

; (sicp)
; (display (format "half-interval-method sin 2.0 4.0 = ~a~%" (half-interval-method sin 2.0 4.0)))
; (display (format "half-interval-method x^3-2x-3=0 1.0 2.0 = ~a~%" (half-interval-method (lambda (x) (- (cube x) (* 2 x) 3)) 1.0 2.0)))

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

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; (sicp)
; (print "fixed-point cos 1.0 = ~a~%" (fixed-point cos 1.0))
; (print "fixed-point y=sin(y)+cos(y) 1.0 = ~a~%" (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))
; (print "fixed-point based sqrt 4.0 = ~a~%" (sqrt 4.0))
; (print "fixed-point based sqrt 11.0 = ~a~%" (sqrt 11.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; (print "average-damp square 10 = " ((average-damp square) 10))
; (print "sqrt 10 = " (sqrt 10))
; (print "cube-root 10 = " (cube-root 10))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

; (print "sqrt 10 = " (sqrt 10))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

; (print "sqrt 10 = " (sqrt 10))

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

; (print "car x = " (car x) ", cdr x = " (cdr x))
; (print "car z = " (car z) ", cdr z = " (cdr z))
; (print "(car (car z)) = " (car (car z)) ", (cdr (cdr z)) = " (cdr (cdr z)))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
; (print-rat one-half)
; (print-rat one-third)
; (print-rat (add-rat one-half one-third))
; (print-rat (mul-rat one-half one-third))
; (print-rat (mul-rat one-third one-third))

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

; (print-rat one-half)
; (print-rat one-third)
; (print-rat (add-rat one-half one-third))
; (print-rat (mul-rat one-half one-third))
; (print-rat (mul-rat one-third one-third))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (cons a b))

; (print "cons 1,2,3,4 = " (cons 1
;              (cons 2
;                    (cons 3 4))))
; (print "list 1,2,3,4 = " (list 1 2 3 4))

(define one-through-four (list 1 2 3 4))
; (print "one-through-four = " one-through-four ", car one-through-four = " (car one-through-four) ", cdr one-through-four = " (cdr one-through-four))
; (print "cons 10 one-through-four = " (cons 10 one-through-four) ", cons 5 one-through-four = " (cons 5 one-through-four))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

; (print "squares at 3 = " (list-ref squares 3))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

; (print "length odds = " (length odds))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

; (print "length odds = " (length odds))
