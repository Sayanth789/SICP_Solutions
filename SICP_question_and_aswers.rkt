;; (Que1) Design of a procedure that evolves an iterative exponentiation process that uses successive 
;squaring and using the logarithmic timing
;;the idea is that , for n = even b ** n = b ^(n/2)*2
; if n is odd , b ** n = b * (b)^(n-1)

#lang racket
 (define (fast-expt-iter b n )
     (define (iter b n a)
         (cond ((= n 0 ) a)
             ((even? n) (iter (* b b ) (/ n 2) a))
             (else (iter b (- n 1) (* a b)))))
         (iter b n 1))
 (fast-expt-iter 2 10)        

;;; ;; Question : Write a log-time multiplication algorithm using
;;; ;; + for addition, double(x) = 2 * x , halve(x) = x / 2
;; The  answer involves using peasant algorithm , means multiplying 2 numbers with only using  adding, 
; doubling , and halving

;;; ; we can use the identity a * b = 2a (b / 2) for b is even 
;;; ; a * b = a + a * (b -1) for odd b

 (define (double x) (+ x x))
 (define (halve x) (/ x 2))

 (define (fast-mul a b)
   (define (iter a b acc)
     (cond ((= b 0) acc)
           ((even? b) (iter (double a) (halve b) acc))
           (else (iter a (- b 1) (+ acc a)))))
   (iter a b 0))
   (fast-mul 18 25)  

               
      

;; The modified version of the Primality checking.Instead of trying higher multiple of 2 , here we check if 
; given no: is a multiple of 2, else we look for 3 , etc(ie: excluding highter multiple of 2)
;; (2, 3, 5,  7, 9...)
(define (square x)
   (* x x))

 (define (divides? a b)
   (= (remainder b a) 0))

 (define (next n)
   (if (= n 2)
       3
       (+ n 2)))

 (define (find-divisor n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor n (next test-divisor)))))

 (define (smallest-divisor n)
   (find-divisor n 2))

 (smallest-divisor 2)
 (smallest-divisor 9)
 (smallest-divisor 1000)

;The carmicheal numbers are  those fools the fermat test, write the procedure that takes an integer n and tests whether 
; a * n is congruent to a modulo n for a < n...and trying some carmichael numbers on them.

 (define (gcd a b)
     (if (= b 0)
         a
         (gcd b (remainder a b))))
 ;; modular exponentiation --as an helper function
 (define (expmod base exp m)
     (cond ((= exp 0) 1)
         ((even? exp)
             (remainder (square (expmod base (/ exp 2) m)) m))
             (else 
                 (remainder (*  base (expmod base (- exp 1) m)) m)))) 
     (define (square x) (* x x))

 ; Fermat test for all a < n , where gcd(a, n) = 1
 (define (fools-fermat? n)
     (define (test a)
         (cond ((>= a n) #t)
             ((= (gcd a n ) 1)
             (if (= (expmod a n n) a)
                 (test (+ a 1))
                 #f))
             (else (test (+ a 1)))))
     (test 2))    ; start from 2, skipping 1

 (display (fools-fermat? 561)) (newline)    
 (display (fools-fermat? 1105)) (newline)  
 (display (fools-fermat? 1729)) (newline)  
 (display (fools-fermat? 2465)) (newline)  
 (display (fools-fermat? 2821)) (newline)  

;; Simpon's rule to calculate the integral more accurately.[h/3 (Y0 + 4Y1 + 2Y2 + 4Y3 + ...+ 2Yn-2 + 4Yn-1 + yn )]
;; where h = (b -a) / n -the greater the 'n' the better the approximation.

;For some integer 'n' and yk = f(a + h* k) define the procedure that  take the arguments 'a','b','f', and 'n that calculate the
; integral using simpson's rule.Use your procedure to calculate the integral of cube b/w 0 and 1 (with taking n = 1000 , 10000)

 (define (simpson-rule f a b n)
     (define h(/ (- b a) n))
    
     (define (term k)
         (define x (+ a (* k h)))
         (define y (f x))
             (cond ((or (= k 0) (= k n)) y)
                 ((odd? k) (* 4 y))
                 (else (* 2 y))))
     (define (loop k sum)
         (if (> k n)
             (* (/ h 3) sum)
             (loop (+ k 1) (+ sum (term k)))))
         (loop 0 0 ))  ; start with 0 and accumulated sum 0
    ; Calculating the intrgarl of f(x) = x ** 3 from 0 to 1
    (define (cube x) (* x x x))
    (simpson-rule cube 0 1 10) ;fn: cube , a = 0 b =1, n =1000 

   ;;;;; Procedure Product - that returns the product of the value of a function at points over a given range.
   ; These exercices shows the power of higher order abstraction
    (define (product term a next b)
        (if (> a b)
            1
            (* (term a)
                (product term (next a) next b)))) 
 ;; and then we define factorial interms of product .. Since n! = n * (n -1) * ... * 1
 (define (identity x) x)
 (define (inc x) (+ x 1))

 (define (factorial n)
     (product identity 1 inc n))

;;; ; Now we use product to find the approximation to the value of pi
 ;;(Pi / 4) = {2n * 2n }   / {(2n -1 )* (2 n + 1) }
 (define (wallis-term n)
     (/ (* 2.0 n) (-(* 2 n) 1))) ;; even part / odd part
 (define (wallis-term2 n)
     (/ (* 2.0 n) (+ (* 2 n) 1))) ;;; even part / next odd

 (define (wallis-pi n)
     (* 2 (product
             (lambda (k) (* (wallis-term k) (wallis-term2 k)))
             1
             (lambda (x) (+ x 1))
             n))) 

 (wallis-pi 100000) ; Approximate pi using 1000 terms                   
 ;;;fixed point approximation.
 (define tolerance 0.00001)

 (define (fixed-point f first-guess)
   (define (close-enough? v1 v2)
     (< (abs (- v1 v2)) tolerance))
  
   (define (try guess)
     (display guess)
     (newline)
     (let ((next (f guess)))
       (if (close-enough? guess next)
           next
           (try next))))
  
   (try first-guess)
 (define (f x)
     (/ (log 1000) (log x)))
 (fixed-point f 2.0)    

;;(Question) --Average-damp procedure --- takes its argument a procedure f and returns as its value as a procedure, that when applied to a number
;; x, produces the average of x and (f x). -- eg: average-damp 10 returns the average of 10 and 100 := 55
 (define (square x) (* x x))
 (define (average-damp f)
      (lambda (x) (average x (f x))))
 ((average-damp square) 10)  
;;Note: Higher order procedures are those (1) Take other procedures as arguments  (2) Returns a procedure as a result.
;; eg: in python - def apply_twice(f, x)
                    ;   return f(f(x)) 
               ;   def square(n):
                   ;   return n * n
        ;          print(apply_twice(square, 2))   # output  

;;implementation of continued fraction, which comverge to the value of 1 / phi

;; Note that cond-frac can be expressed as: f(x) = { Ni / Di + F(i +1) if i <= k,  0 else

 (define (cont-frac n d k)   ;; Where k is the limit
   (define (recurse i)
     (if (> i k)
         0
         (/ (n i) (+ (d i) (recurse (+ i 1))))))
   (recurse 1))

 (define k 5) ; or whatever value you prefer
 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)
