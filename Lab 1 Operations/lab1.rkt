#lang eopl
#| In each lab, you will implement a series of functions.
 | Each function you successfully implement will contribute
 |   some number of points to your total grade.
 | The point value of each function is stated above its declaration.
 | Each function will be assessed with a set of test cases which
 |   may NOT be the same as provided test cases.
 |#




#|-------------------------------------------------------------------------------|
 |                        Lab 1: Operations (20 PTS)                             |
 |-------------------------------------------------------------------------------|#

#| In this lab, you'll implement some basic operations on integers and booleans.
 | The following built-in functions will prove useful:
 |   (+ x y)   returns x + y.
 |   (* x y)   returns x * y.
 |   (/ x y)   returns x / y.
 |   (sqrt x)  returns the square root of x.
 |   (not p)   returns ¬p.
 |   (and p q) returns p ∧ q.
 |   (or p q)  returns p ∨ q.

 | Once you've successfully implemented a function,
 |   you can use it in subsequent functions too! 

 | True and false are written in Scheme as "#t" and "#f" respectively.
 |#




#|-------------------------------------------------------------------------------|
 |                          Part 1: Arithmetic (8 PTS)                           |
 |-------------------------------------------------------------------------------|#

#| Implement "average" to accept integers x1, x2, and x3,
 |   and return the average of all three integers: (x1 + x2 + x3) / 3
 | Examples:
 |   (average 1 2 3)       ->   2
 |   (average -10 15 -20)  ->  -5
 |   (average -7 32 15)    ->  13.333...
 |   (average 76 145 -349) -> -42.666...
 |#

;; Type signature: (average int int int) -> number
;; 2 PTS
(define (average x1 x2 x3)
  (/(+ x1 x2 x3) 3))



#| Implement "hypotenuse" to accept integers a and b,
 |   and return the hypotenuse c of a triangle with legs a and b.
 | c = sqrt(a^2 + b^2).
 | Examples:
 |   (hypotenuse 3 4)   ->  5
 |   (hypotenuse 28 45) -> 53
 |   (hypotenuse 1 1)   ->  1.41421...
 |   (hypotenuse 6 15)  -> 16.15549...
 |#

;; Type signature: (quadratic int int) -> number
;; 3 PTS
(define (hypotenuse a b)
 (sqrt (+ (* a a) (* b b))))




#| Implement "quadratic" to accept integer coefficients a, b, c,
 |   and integer variable x, and return a*x^2 + b*x + c.
 | Examples:
 |   (quadratic 0 1 5 -2)      ->    3
 |   (quadratic 1 -1 1 7)      ->   43
 |   (quadratic -5 9 -4 3)     ->  -22
 |   (quadratic 35 -223 74 16) -> 5466
 |#

;; Type signature: (quadratic int int int int) -> int
;; 3 PTS
(define (quadratic a b c x)
  (+ (* a (* x x)) (* b x) c))




#|-------------------------------------------------------------------------------|
 |                          Part 2: Truth Tables (12 PTS)                        |
 |-------------------------------------------------------------------------------|#

#| Implement "nand" so that it accepts two booleans p and q
 |   and returns p ⊼ q [p nand q]:
 |
 | p q │ p ⊼ q
 | ────┼───────
 | T T │ F
 | T F │ T
 | F T │ T
 | F F │ T
 |
 | Example of testing this function:
 |   (nand #t #f) -> #t
 |#

;; Type signature: (nand boolean boolean) -> boolean
;; 2 PTS
(define (nand p q)
  (not (and p q)))




#| Implement "implies" to accept two booleans p and q
 |   and return "p ⇒ q" [if p, then q]:
 |
 | p q │ p ⇒ q
 | ────┼───────
 | T T │ T
 | T F │ F
 | F T │ T
 | F F │ T
 |#

;; Type signature: (implies boolean boolean) -> boolean
;; 2 PTS
(define (implies p q)
 (or (not p) q))




#| Implement "xor" to accept two booleans p and q
 |   and return p ⊕ q [p exclusive or q]:
 |
 | p q │ p ⊕ q
 | ────┼───────
 | T T │ F
 | T F │ T
 | F T │ T
 | F F │ F
 |#

;; Type signature: (xor boolean boolean) -> boolean
;; 2 PTS
(define (xor p q)
  (and (implies p q) (implies q p)))




#| Implement "3majority" to return #t iff
 |   a majority of its three arguments are #t:
 |
 | p q r │ (3majority p q r)
 | ──────┼──────────────────
 | T T T │ T
 | T F T │ T
 | F T T │ T
 | F F T │ F
 | T T F │ T
 | T F F │ F
 | F T F │ F
 | F F F │ F
 |#

;; Type signature: (3majority boolean boolean boolean) -> boolean
;; 3 PTS
(define (3majority p q r)
 (or (or (and p q) (and p r) ) (and q r)))




#| Implement "isosceles" to return #t when
 |   exactly two of its arguments are #t:
 |
 | p q r │ (isosceles p q r)
 | ──────┼──────────────────
 | T T T │ F
 | T F T │ T
 | F T T │ T
 | F F T │ F
 | T T F │ T
 | T F F │ F
 | F T F │ F
 | F F F │ F
 |#

;; Type signature: (isosceles boolean boolean boolean) -> boolean
;; 3 PTS
(define (isosceles p q r)
   (and (3majority p q r) (or (or (not p) (not q)) (not r) ) ))
