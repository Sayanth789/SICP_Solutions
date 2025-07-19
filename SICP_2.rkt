;;; These are some questions and answers from chapter 2, also included some useful functions from the text.


;A better version of make-rat  that handles positive and negative arguments.If rat is positive, both nr and and dr
; are positive, and if the rational no: is negative, only numerator is negative.
; Note: make-rat is a constructor procedure used to create a rational number, typically represented as a pair of integers: a numerator and a denominator.
; As an eg:  (make-rat 4 8)  ; returns 1/2 (simplified)



(define (make-rat n d)
  (let* ((g (gcd n d)) ; <-- let* not let *(there'll be no space.)
         (n (/ n g))
         (d (/ d g)))
    (if (< d 0)
        (cons (- n) (- d))    ; flip the sign if denominator is negative
        (cons n d))))         ; use gcd to simplify the rational number
(define (numer x) (car x)) ; Get the numerator
(define (denom x) (cdr x))

(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline))
(define r (make-rat 2 4))
(print-rat r)

;;;Que : To represent a line segment in a plane, each segment is represented as a pair of points: a starting and an ending. 
;;;  Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segmentation 
;;; interms of points.
(define (make-point x y)
    (cons x y))

; Selectors for a point
(define (x-point p)
    (car p))
(define (y-point p)
    (cdr p))

;;Que:  A point can also be represented as a pair of numbers: x-coor and y-cord.Thus specify a constructor make-point 
;; and selectors x-point and y-point that define this representation.
;; Constructor for a segment

(define (make-segment start-point end-point)
    (cons start-point end-point))

;; selectors for a segment
(define (start-segment s)
    (car s))

(define (end-segment s)
    (cdr s))

;; Que:Using the selectors and constructors define a procedure midpoint-segment that takes a line segment as
; argument and returns its midpoint(the point whose coords: are the average of the coord: of the endpoints).
;ie midpoint_x = (X1 + X2) /2 and midpoint_y = (y1 + y2)/ 2

;;Midpoint  of a segment

(define (midpoint-segment s)
    (let  ((start (start-segment s))
         (end (end-segment s)))
      (make-point 
       (/ (+ (x-point start) (x-point end)) 2)
       (/ (+ (y-point start) (y-point end)) 2))))
(define p1 (make-point 0 0))
(define p2 (make-point 4 7))

(define seg (make-segment p1 p2))

(midpoint-segment seg) ;; should give (2 . 2)
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))
(define p1 (make-point 0 0))
(define p2 (make-point 4 7))
(define seg (make-segment p1 p2))

(define (print-segment segment)
  (print-point (start-segment segment))
  (display " to ")
  (print-point (end-segment segment)))


(print-segment seg)


;; Question: Implement a representation for rectangles in a plane. Interms of your constructor and selectors
;create a procedure that compute the perimeter and area of the given rectangle.

(define (make-rectangle p1 p2)
    (cons p1 p2)) ; p1 and p2 are points

(define (make-point x y) 
    (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
;; Defining the selectors

(define (bottom-left r) (car r))
(define (top-right r)  (cdr r))

(define (rectangle-width r)
    (let ((bl (bottom-left r))
        (tr (top-right r)))
    (- (x-point tr) (x-point bl))))

(define (rectangle-height r) 
    (let ((bl (bottom-left r))
        (tr (top-right r)))
     (- (y-point tr) (y-point bl))))

(define (rectangle-area r)
    (* (rectangle-width r) (rectangle-height r)))
;;Usage

(define p1 (make-point 0 0)) ;; bottom-left
(define p2 (make-point 4 7)) ;; top-right

(define rect (make-rectangle p1 p2))

(rectangle-area rect) ;; 28  === 4* 7

;; A different representation for rectangle
;;; -------------------------------------------------------------
(define (make-rectangle2 corner2 width2 height2)
  (cons corner2 (cons width2 height2)))

(define (rectangle-corner2 rect)
  (car rect)) ;; get the point

(define (rectangle-width2 rect)
  (car (cdr rect))) ;; get the width

(define (rectangle-height2 rect)
  (cdr (cdr rect))) ;; get the height

(define (rectangle-area2 rect)
  (* (rectangle-width2 rect)
     (rectangle-height2 rect))) ;; fixed here
(define (make-point x y) 
    (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;; Defining the selectors

(define p (make-point 0 0))
(define rect (make-rectangle2 p 4 7))

(rectangle-area2 rect)   ;; returns 28
; Here we used the idea that rect: can be represented as Center-point + half-width + half-height
;;; ---------------------------------------------------------------------------------------------------


;; Que:  Define a procedure last-pair that returns only the last element of a given nonempty list: (Last-pair (list 23 72 149 34 )) - will return (34)

(define (last-pair lst)
    (if (null? (cdr lst))
         lst
        (last-pair (cdr lst))))
(last-pair (list 23 72 149 34))         

;;Defining the procdure reverese-list that take a list as arguments and returns the revesed order of 
;; the list ----we use the successive cdr procedure and last  do the 'car' ing
;; The term append will append the first element to the list (car lst) to the end
(define (reverse-list lst)
  (if (null? lst)
      '()
      (append (reverse-list (cdr lst)) (list (car lst)))))

(reverse-list (list 1 2 3 4))


;;For the problem: count-change --we want to make change for an amount using coins of certain denominations.Instead of hard-coding the
;; coin denominations , we pass them as a list.(note: the coin denomination is a popular in CS and mathematics, it involve finding 
;;; ways to make change for a given amount of money using a set of coin denominations).
;;; -----------------------------------------------------------------------------------------------------------------------------
(define (cc amount coin-values)
    (coin ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                 (except-first-denomination coin-values))
            (cc (- amount 
                     (first-denomination coin-values))
                 coin-values)))))

;; here we define the procedures first-denominations, except-first-denominations,and no-more?
(define (first-denominations coin-values)
    (car coin-valuess))   ;; returns the first coin

(define (except-first-denomination coin-values)
    (cdr coin-values))    ;; return the rest of the coins
;; Eg: For a list (30, 40 , 50)  ....it returs the list (40, 50)
(define (no-more? coin-values)
    (null? coin-values))   ;; true if no coins left.;; checks if the list is empty.


;; In a procedure definition a parameter list that has a dot before the last parameter name indicates that, when the procedure is called,
;the initial parameter will have  as many values as the initial parameters, as usual but the final parameters value will be a list of
; any remaining arguments. Eg: For the  given def: (define (f x y . z) <body>)  the procedure f can be called with 2 or more args.
;; (f 1 2 3 4 5 6)  then fx =  1 , fy = 2 , and fz = (3 4 5 6)

; Use the notation to write a procedure same-parity that takes 1  or more arguments and return a list of all the arguments that have the same even-odd parity
;; as the first arg. For eg: (same-parity (1 2 3 4 5 6 7)) == (1  3 5 7)
;(same-parity 2 3 4 5 6 7 ) == (2 4 6)
;; Here the input list matches the parity with the parity of first element in the given list.

(define (same-parity x. rest)
    (define parity-check
        (if (even? x) even? odd))
    (cons x (filter parity-check rest)))

;;; parity-check becomes either even? or odd? depending on x.
;;; (filter parity-check rest) filters the  list to only elements of that parity.
;;; (cons x ....) add x to the front.
;____________________________________________________________________________________________________________

; The recursive version of square list is:

(define (square-list items)
    (if (null? items)
      '()
        (cons (* (car items) (car  items))   ;; For the first item
               ( square-list (cdr items)))))  ;; for the second item...
(display (square-list  (list 1 2 3 4))) 
(newline)    

;;The map function applies to every element in the list and returns a new list of the results
;;using the map function for square ----we get
(define (square-list1 items)
    (map (lambda (x) (* x x)) items))

(display (square-list1 (list 1 2 3 4)))
(newline)

;; Modification of the reverse procedure : to produce a deep-reverse , that take a list as argument
;and returns its value.The list both the elements reversed and with all sublists deep-deversed as well.
; Eg: (define x (list (list 1 2) (list 3 4))) ..x = ((12) (3 4)) then reverse x = ((3 4) (1 2))
; deep-reverse x = ((4 3 ) (2 1))

;The deep-list not only reverse the-list but also the sub-list in it.
(define (deep-reverse items)
    (if (null? items)
    '()
    (append (deep-reverse (cdr items))
        (list (let ((x (car items)))
                (if (pair? x)  ; if  x is a sublist
                    (deep-reverse x)  ; recures in the subilst
                     x))))))
(define x (list (list 1 2) (list 3 4)))
(reverse x) 
(deep-reverse x)                    
