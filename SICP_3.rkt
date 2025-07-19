#lang racket

; Define a procedure fringe that take as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left to right order
; Eg: (define x (list (list 1 2)(list 3 4)))  ( fringe x) == 1 2 3 4, (fringe (list x x)) = (1 2 3 4 1 2 3 4)

; If the tree is empty, we return '() (empty list)..If the car is a list, recursively fringe it
; if the car is not a list (a leaf) , collect it .Always recurse on (cdr tree) too
(define (fringe tree)
    (cond ((null? tree) '())   ; empty tree -> emptylist
           ((pair? (car tree))    ; if car is a list
             (append (fringe (car tree))
                     (fringe (cdr tree))))  ; flatten car + cdr
             (else (cons (car tree)
                        (fringe (cdr tree))))))  ; car is a leaf

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list  x x))
;;; -------------------------------------------------------------------------------------------------------------------------------------------------
;; A Binary mobile has 2 branches left and right ...in which left and right branches(each branches is a rod of certain length) can either contain a weight or another
; binary moblile. The binary mobile is defined as: (define (make-mobile left right)  (list left right))
;a branch is costructed from a length (which must be a number) together with a structure, which may either a number(representing a simple weight) or another mobile.

(define (make-branch length structure) (list length structure))
(define (make-mobile left right) ; mobile is represented as a list of 2 branches
    (list left right))

(define (left-branch mobile)
    (list car mobile))

(define (right-branch mobile)
    (list cadr mobile))
;; Branch is represented as a list; (length structure)
(define (make-branch length structure) 
    (list length structure))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cadr branch))        
;;;Using your selectors ...define a procedure total-weight that returns the total weight of the mobile.
(define (total-weight structure)
    (if (number? structure)
         structure
    ( + (total-weight (branch-strurcture (left-branch structure)))
        (total-weight (branch-structure  (right-branch structure)))))
)

;; Part c --> A mobile is balanced if the torque applied by its left branch is equal to that applied to right , and if each branch is balanced.
;;; ; Torque = length x total-weight
(define balanced left-torque right-torque mobile);; Define can only define one thing at a time.
     (define  left-torque right-torque branch-length total-weight)
         
         (left-torque (= (* left-branch-length total-weight)))
         (right-torque (= (* right-branch-lenth total-weight)))
         (if (= left-torque right-torque)
             balanced
         (else....)))

;; use (define (name args ....) body ) for procedures
;; use (let ((var val1) (var2 val2)) body)  to define local vars
;; use and to combine multiple conditions.......thus the answer is

(define (balacned? mobile)
    (let* ((left (left-branch mobile))
    (right (right-branch mobile))
    (left-torque (* (branch-length left)
                     (total-weight (branch-structure left))))
    (right-torque (* (branch-length right)
                     (total-weight (branch-structure right)))))
    (and (= left-torque right-torque)
         (balanced-branch? (branch-structure left))
         (balaced-branch? (branch-structure right)))))                                  
    
;;And we define the balanced-branch?  as:
(define (balanced-branch? structure)
    (if (number? structure) ;; simple weight? always balacnced
        #t 
        (balanced? structure)))  ;;if, mobile check recursively

Defining a procedure square-tree annalgous to the square-list procedure.
(define (square-tree tree)
    (cond ((null? tree) '())  ; ; empty tree 
           ((pair? (car tree))  ;first element is a list
           (cons (square-tree (car tree))   
                 (square-tree (cdr tree))))
             (else   ; first element  is a number
              (cons (* (car tree) (car tree))
                    (square-tree (cdr tree))))))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;Here's how it work: 1  is a num: thus 1 square = 1 , (list 2 (list 3 4) 5) = list: thus recurse , 2 is a number thus square, list(3 4) = a list, thus we recurrse....and so on

;; The same can be done, more cleanly using the map and recursion
(define (square-tree tree)
    (map (lambda (x)
           (if (pair? x)
               (square-tree x)   ;; if x is a list, recurse
               (* x x)))
            tree))
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))        

;;; ___________________________________________________________________________________________________________________________________________
;; Define a function tree-map that takes argument  a procedure f and a tree(represented as a list) and returns a tree with the same shape, but with each number replaced by the result of applying f to that number.
; ie: needed to map a function over every number in a tree-like structure.
(define (tree-map f tree)
    (cond ((null? tree) '())
    ((not (pair? tree)) (f tree)) ; if it's a number(leaf) , apply f
    (else (cons (tree-map f (car tree))
                (tree-map f (cdr tree)))))) 
(define tree (list 1 (list 2 (list 3 4) 5) 6))                
(tree-map (lambda (x) (* x x)) tree)

;; For a given set the code will generate the list of subsets..
(define (subsets s)
    (if (null? s)
        (list '())  ; Base case: only one subset of empty list is empty list
        (let ((rest (subsets (cdr s))))
         (append rest
             (map (lambda (x) (cons (car s) x))
             rest)))))
(subsets (list 1 2 3))             

;;----Filtering :of a sequence to select only those element that satisfy the given predicate is:
(define (filter predicate sequence)
  (cond ((null? sequence) '())  ; empty list is '()
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 6))

;;; Accumulation can be implemented as

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5 6 7 8 9))   
(accumulate * 1 (list 1 2 3 4 5 6))         
(accumulate cons '() (list 1 2 3 4 5))

(define (enumerate-interval low high)
    (if (> low high)
    '()
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 8)      

;;Thus to enumerate the leaves of a tree, we can use:
(define (enumerate-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))                        
;;; ---------------------------------------________________________________________________--------------------------------------------------------------
;;; We can evaluate the polynomial an x^n + [a (n-1)* x^(n-1) ] + ...+ a1x + a0  ..using the well-known algorithm known as
;;; Horner's Rule , which sttuctures the computation as: (...(anx + an-1)x + ... + a1)x + a0
;;; i.e. start with a multiply by x, add a(n-1) , multiply by x and so on. Fill in the following procedure that evaluate the Horne's rile.
;;; Assume the coefficient of the polynomials are arranged in the order from a0 to an.
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff highest-terms) 
                  (+ this-coeff (* x higher-terms))) 
                0
                coefficient-sequece))
;;; Here to calculate 1 + 3x + 5x^3 + x^5 -----at  x =2 you would evaluate (horner-eval 2 (list 1 3 0 5 0 1))


                
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree)))))
;;; redefine the count-leaves as an accumulation-- Here we use map and accumulation.
(define (count-leaves t)
    (accumulate + 
                0 
                (map (lambda (subtree)
                        (if (pair? subtree)
                             (count-leaves subtree)
                             1))
                        t)))
;; the procedure maps over each subtree, If the tree is a pair,recursively count the leaves, If the subtree is a leaf (not a pair)
;; just return 1, Finally accumulate (+) all those 1s together to get the total count.

;;; The full varsion of the accumulate-n (which is similar to accumulate except the 3rd argument is sequence of sequences, which are all assumed to have the
;;; same number of elements ).
(define (accumulate-n op init seqs) ; here the argument seqs is a sequence of sequece
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map car seqs))
               (accumulate-n op  init (map cdr seqs)))))

;;; ;;;  Suppose we represent the vectors v = vi as a sequence of numbers and the matrices mi = (mij) as sequcence of vectors.

;;; We can express the dot product as:
(define (dot-product v w)
    (accumulate + 0 (map * v w)))

;;; Fill in the missing expression in the following procedures for computing the other matix operations.

;;; (define (matrix-*-vector m v)
;;;     (map (lambda (row) (accumulate + 0 (map * row v)))
;;;            m))

;;; (define (transpose mat)
;;;   (if (null? (car mat))         ; if the first row is empty
;;;       '()                       ; done
;;;       (cons (map car mat)        ; take first column
;;;             (transpose (map cdr mat))))) ; recur on rest
;;; ;;; We can make a better version of it by using the accumulate-n
;;; (define (transpose mat)
;;;     (accumulate-n (lambda (x y) (cons x y)) '() mat))

;;; (define(matrix-*-matrix m n)
;;;     (let ((cols (transpose n)))
;;;     (map (lambda (row)
;;;       (map (lambda (col)
;;;                (dot-product row col))
;;;                cols))    
;;;        m)))        
;; Complete the following definition of reverse in terms of fold-right and fold-left from the book:
;;; ; the completed answer is:
(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) nil sequence)) 
(define (reverse sequence)
    (fold-right (lambda (x y)) (cons y x) nil sequence))       






; function to filter  the sequence of pairs to find those whose sum is a prime.
(define (prime-sum pair)
    (prime? (+ (car pair) (cadr pair))))
; procedure for generating the sequence of results by mapping over the filtered pairs.
(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+(car pair) (cadr pair))))

; Combining all these steps yields the complete procedure
(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
            (flatmap
            (lambda (i)
               (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))    

; define  a unique-pair that , given an integer n, generates the sequence of pair (i, j) such that 
; 1 <= j < i <= n. Use unique-pair to simplify the definition of the prime-sum-pairs.  


(define (enumerate-interval  low high)
    (if (> low high)
        '()
        (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
    (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))        

(define (flatmap proc seq)
    (accumulate append '() (map proc seq))) 

(define (prime? n)
    (define (divides? a b) (= (remainder b a) 0))
    (define (has-divisor? a)
        (cond ((> (* a a) n) #f)
            ((divides? a n) #t)
            (else (has-divisor? (+ a 1)))))
        (cond ((< n 2) #f)
            (else (not (has-divisor? 2)))))
(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
    (flatmap
     (lambda (i)
      (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))    

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
            (unique-pairs n))))          


;;Question: Write a procedure to find all ordered triples of distinct positive integers: i , j ans k less than 
; or equal to a given integer n and sum to a given integer s.


; Here we're given i, j, k and need to generate the list of all ordered triples of distinct positive integes(i, j,k)
; such that: i < j< k <= n and i + j + k = s for some n and s.Thus we  generate all ordered triples i < j < k <= n.Filter those whose sum is equal to 's'

(define (ordered-triples n)
    (flatmap
     (lambda (i)
      (flatmap 
       (lambda (j) 
        (map (lambda (k) (list i j k))
            (enumerate-interval  (+ j 1) n)))
        (enumerate-interval (+ i 1) n)))
    (enumerate-interval 1 (-  n 2))))     

; outerloop :i from 1 to n - 2
; middle: j from i + 1 to n - 1 , inner: k  from j + 1 to n.This ensure i < j < k.

(define (sum-equals? s triple)
    (= (+ (car triple)
           (cadr triple)
           (caddr triple))
           s))

(define (triple-sum n s)
    (filter (lambda (triple) (sum-equals? s triple))
             (ordered-triples n)))

;;; ; Eg Usage 
(triple-sum 7 10)

;;;   ----------------------------------------------------------
;;; a flatmap equivalent in python is:
;;; def unique_pairs(n):
;;;     pairs = []
;;;     for i in range(1, n+1):   # i from 1 to n 
;;;         for j in range(1, i):  # j from 1 to i - 1
;;;             pairs.append([i, j])    # collects pairs [i, j]
;;;     return pairs        
;;; -----------------------------------------------------------------

;;; complete procedure 8 queens. 
;;; Note: The 8 Queens Problem is a classic backtracking problem in computer science and chess puzzle
;;; Place 8 queens on a standard 8×8 chessboard so that no two queens threaten each other.

;;; This means: No two queens share the same row, No two queens share the same column, No two queens share the same diagonal

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
             (lambda (positions) (safe? k positions))
             (flatmap
              (lambda (rest-of-queens)
                  (map (lambda (new-row)
                           (adjoin-position new-row k rest-of-queens))
                        (enumerate-interval 1 board-size)))
                   (queen-cols (- k 1))))))
        (queen-cols board-size))           

;;; rest-of-queens is a way to place k - 1 queens in the first k - 1 colomns, and new-row is the proposed row in which to place the queen 
;;; in the k th colomn.
;;; complete the program including the sets of board positions, inclucding procedure adjoin-position, empty-board, procedure 'safe?'---  which
;;; determine for a set of positions , whether queen in 'k' th colomn is safer w.r.t. others.


(define (enumerate-interval low high )
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
    (cond ((null? sequence) '())
        ((predicate (car sequence))
        (cons (car sequence) (filter predicate (cdr sequence))))
       (else  (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (accumulate op initial sequence)
    (if (null? sequence)
    initial 
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (adjoin-position new-row k rest-of-queens)
    (cons (list new-row k) rest-of-queens))        


;;; ;;; checking for safety:

(define (safe? k positions)
  (define (conflict? pos1 pos2)
    (let ((r1 (car pos1)) (c1 (cadr pos1))
          (r2 (car pos2)) (c2 (cadr pos2)))
      (or (= r1 r2)                          ; same row
          (= (- r1 c1) (- r2 c2))           ; same major diagonal
          (= (+ r1 c1) (+ r2 c2)))))        ; same minor diagonal
  (define (check pairs)
    (cond ((null? pairs) #t)
          ((conflict? (car pairs) (car positions)) #f)
          (else (check (cdr pairs)))))
  (check (cdr positions)))



;;; Main function to sove the problem:
(define (queens n)
    (define (queen-cols k)
        (if (= k 0)
            (list '())
            (filter 
             (lambda (positions) (safe? k positions))
              (flatmap 
               (lambda  (rest-of-queens)
                (map (lambda (new-row)
                        (adjoin-position new-row k rest-of-queens))
                     (enumerate-interval 1 n )))
                (queen-cols (- k 1))))))
    (queen-cols n))
(queens 8)
;;; -------------------------------------------------------------------------------------------------------------------------------
;; The decoding procedure of Hauffman encoding tree.

(define (decode bits tree) 
    (define (decode-1 bits current-branch)   ;; this procedure takes 2 args,list of remaining bits and current position.
        (if (null? bits)
            '()
            (let ((next-branch
                   (choose-branch (car bits) current-branch)))
                   (if (leaf? next-branch)   ;; if next-branch is the  leaf, add the symbol.(ie, we decoded a symbol)
                       (cons (symbol-leaf next-branch)
                             (decode-1 (cdr bits) tree)) ;; then recursively decode the tree from the start.(Here we go to the tree not to the next-branch!)
                       (decode-1 (cdr bits) next-branch))))) ;; else; we recurse for the next-branch
        (decode-1 bits tree))

    (define (choose-branch bit branch)
        (cond ((= bit 0) (left-branch branch))  ; if bit is '0', then follow the left-branch
        ((= bit 1) (right-branch))  ;; if the bit is '1' , then follow the right branch.
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))                   
;;____________________________________________________________________


;; Given an operation op and some args (which may've different types), figure out right fn: to apply based on 
;; their types, and call it. 
;; It supports polymorphism (same op works in different types), It also supports type 'coresion'.

(define (apply-generic op . args)  ;; the op can be (add, mul, etc.), args is list of arguments(complex no; rational)
    (let ((type-tags (map type-tags args)))  ; extracts type-tag from ecah argument.
      (let ((proc (get op type-tags)))  ;; use get to find if there's an operation defined for op and these type-tag
        (if proc      ;; if we found a procedure apply it
            (apply proc (map contents args))
            (if (= (length args) 2)    ;; it only tries cohesion if there are 2 args.
                (let ((type1 (car type-tags))
                      (type2 (cadr type-tags))
                      (a1 (car args))
                      (a2 (cadr args)))
                    (let ((t1->t2 (get-corecion type1 type2))  ; get coresion tries to find fn: that converts from type1 to type2
                         (t2->t1 (get-corecion type2 type1)))   ;; or type2 to type1
                     (cond (t1->t2   ;; if coresion from type1->type2 exists.
                         (apply-generic op (t1->t2 a1) a2))  ; convert a1 to type2
                         (t2->t1
                         (apply-generic op a1 (t2->t1) a2))) ;; retry apply generic eith converted args
                        (else 
                         (error "No method for these types"
                                    (list op type-args)))))
                     (error "No method for these types"
                           (list op type-tags)))))))