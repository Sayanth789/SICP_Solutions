#lang racket

;; Check if a number is prime
(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor test-divisor)
      (cond ((> (* test-divisor test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor (+ test-divisor 1))))) ;; FIX: find-divisor needs argument
    (find-divisor 2))

  (define (divides? a b)
    (= (remainder b a) 0))    

  (= n (smallest-divisor n))) ;; n is prime if smallest-divisor is n itself

;; Timing the prime test
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
       'not-prime)) 
(define (report-prime elapsed-time)
  (display " **** ")
  (display elapsed-time)
  (display " ms"))

;; Finding the first 3 primes in a given range
(define (search-for-primes start end)
  (define (search n count)
    (cond ((= count 3) 'done)      ;; Found 3 primes, stop
          ((> n end) 'done)        ;; Reached end, stop
          (else 
            (begin 
              (if (prime? n)
                  (begin 
                    (timed-prime-test n)
                    (search (+ n 1) (+ count 1))) ;; FIX: increment count when prime found
                  (search (+ n 1) count))))))      ;; else just increment n
  (search start 0))

;; Finding the primes within the given range
(search-for-primes 1000000 10000000) ;; FIX: need two arguments
