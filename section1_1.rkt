#lang sicp
; Exercise 1.1:
10 ; Result: 10
(+ 5 3 4) ; Result: 12
(- 9 1) ; Result: 8
(/ 6 2) ; Result: 3
(+ (* 2 4) (- 4 6)) ; Result: 6
(define a 3) ; Result: nothing, up to interpeter
(define b (+ a 1)) ; Result: nothing, up to interpeter
(+ a b (* a b)) ; Result: 19
(= a b) ; Result: #f (false)
(if (and (> b a) (< b (* a b)))
    b
    a) ; Result 4 (b)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; Result: 16
(+ 2 (if (> b a) b a)) ; Result: 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; Result: 16

; Exercise 1.2
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

; Exercise 1.3
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (>= x y)
  (not (< x y)))

(define (sum-squares-top-2 x y z)
  (cond ((and (>= y x) (>= z x)) (sum-of-squares y z))   ; x smallest
        ((and (>= x y) (>= z y)) (sum-of-squares x z))   ; y smallest
        ((and (>= x z) (>= y z)) (sum-of-squares x y)))) ; z smallest
 
(sum-squares-top-2 1 2 3)
(sum-squares-top-2 2 3 1)
(sum-squares-top-2 -1 -2 3)

; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; The operator for the combination is (if (> b 0) + -))
; Evaluating this conditional expression gives us + if b > 0 and - otherwise
; So we have a procdure that evaluates - a b if b <=0 and + a b otherwise
; I.e. a + abs(b)
(a-plus-abs-b 3 -56)

;Exercise 1.5
;(define (p) (p))
;(define (test x y)
;  (if (= x 0)
;      0
;      y))
;(test 0 (p))
; With an interpreter that uses applicative-order evaluation, the interpreter will
; be stuck recursing as it evaluates (p) which 

; With an interpreter that uses normal-order evaluation, the procedure will return 0.
; this is because the expanded expression is
; (f  (= x 0)
;     0
;     (y)))
; x = 0, so y will never be evaluated (and hence no infinite regress) and the consequent will be evaluated.

; Code from section:
(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

; Always start out a guess with 1.0 for Newton's method
(define (sqrt x)
  (sqrt-iter 1.0 x))

;Exercise 1.6
; The problem with Alyssa's new-if procedure is due to Scheme's applicative-order evaluation.
; Every parameter will be evaluated, including the else-clause even if the predicate is true.
; This might have some side-effets and in Alyssa's case when defining sqrt-iter will result
; in an endless recursion as calls to sqrt-iter will be made ignoring the good-enough predicate.
; Here's it in action:
;
;(define (new-if predicate
;                then-clause
;                else-clause)
;  (cond (predicate then-clause)
;        (else else-clause)))
;(new-if (= 2 3) 0 5)
;(new-if (= 1 1) 0 5)
;
;(define (sqrt-iter-alt guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter-alt (improve guess x) x)))
;(sqrt-iter-alt 1 5)
;
; if is a special form, not a procedure because the "parameters"(operands) are not necessarily all evaluated

;Exercise 1.7

; For a very small number, the square root is itself very small.
; So our good-enough value of 0.001 might define too large a range of acceptable values
; For example:
(sqrt 0.0005)
(square (sqrt 0.0005))
; The square of our final guess is indeed within 0.001 of 0.0005, but the relative distance
; is very large. I.e.
(/ (square (sqrt 0.0005)) 0.0005)
; its square is 2.65 times as large as the actual value its square should be

; Machines have limited precision, so with very large numbers we run into some problems.

; Take a very large number like 2 x 10^(20) for instance:
;(sqrt 2e20)
; It will run infinitely as it will never reach a guess that is good enough in terms of
; our current bound.
; The actual value is sqrt(2) * 10^(10).
; If you look at the history of the guesses, eventually the procedure will reach
; 14142135623.73095. The thing with floating point numbers is that the "spaces"
; between possible numbers increase as the represented numbers get larger. So we can
; reach a point where there is no number between guess and x/guess

; guess = 14142135623.73095
; x/guess = 2e20/guess = 14142135623.730951
; average = 14142135623.73095
;
; but
; (abs (- (square 14142135623.73095) 2e20)) = 32768 > 0.001
; So we will be stuck in an endless recursion.
; A way to counter this is to have a stopping condition that recognizes if the guesses
; are unchanging.


; Better version:

(define (good-enough-2? old_guess guess)
  (< (/ (abs (- guess old_guess)) guess) 0.0001))

(define (sqrt-iter-2 guess x)
  (if (good-enough-2? guess (improve guess x))
      (improve guess x)
      (sqrt-iter-2 (improve guess x) x)))

(define (sqrt-2 x)
  (sqrt-iter-2 1.0 x))

(sqrt-2 0.0005)

; 1.8
; Newton's method for cube roots:


(define (cubert x)
  (define (good-enough? old_guess guess)
    (< (/ (abs (- guess old_guess)) guess) 0.0001))
  
  (define (improve-cube guess)
    (/ (+
        (/ x (square guess))
        (* 2 guess))
       3))
  
  (define (cubert-iter guess)
    (define improved-guess (improve-cube guess))
    (if (good-enough? guess improved-guess)
        improved-guess
        (cubert-iter improved-guess)))
  
  (cubert-iter 1.0))

 (cubert 8)

