#lang eopl
#|-------------------------------------------------------------------------------|
 |                        Part 1: Conditionals (2 PTS)                           |
 |-------------------------------------------------------------------------------|#

#| Like in other languages, the main way to implement a conditional branch
 |   is with an "if-then-else" statement.
 | In Racket this is done with the function (if condition then else),
 |   where "condition" is an expression which evaluates to a boolean.
 | For example, (if (= 1 2) (+ 3 4) (+ 5 6)) will evaluate to 11.
 |
 | "if" functions can be nested to emulate the "else if/elif" of other languages.
 |
 | You may want to look into Racket's "cond" function for some parts of this assignment.
 | cond allows for multiple conditions in a row, effectively condensing
 |   a bunch of nested if statements into one statement.
 |#




#| Implement "piecewise" to accept an integer x
 |   and return 2*x^2 + 1 if x is positive,
 |   and x + 1 otherwise.
 | Examples:
 |   (piecewise -5) -> -4
 |   (piecewise -2) -> -1
 |   (piecewise 0)  -> 1
 |   (piecewise 3)  -> 19
 |   (piecewise 6)  -> 73
 |#

;; Type signature: (piecewise int) -> int
;; 1 PTS
(define (piecewise x)
  (cond
    [(< 0 x) (+ (* 2 (expt x 2)) 1)]
    [else (+ x 1)]))






#| Implement "sign" to accept an integer x
 |   and return "positive" if x is positive,
 |   "negative" if x is negative,
 |   and "zero" if x is zero.
 | Examples:
 |   (sign 5)  -> "positive"
 |   (sign 0)  -> "zero"
 |   (sign -2) -> "negative"
 |#

;; Type signature: (sign int) -> string
;; 1 PTS
(define (sign x)
  (cond
    [(< x 0) (write "negative")]
    [(> x 0) (write "positive")]
    [(= x 0) (write "zero")]))




#|-------------------------------------------------------------------------------|
 |               Part 2: Arithmetic Recursion (9 PTS) (+2 EC PTS)                |
 |-------------------------------------------------------------------------------|#

#| Here, we'll implement some arithmetic functions with recursion.
 | For each function, consider the base case of the recursion
 |   and use an "if" or "cond" statement to catch the base case.
 | In Racket, to call a function recursively you can simply call the function
 |   from inside of itself.
 | Below is an example function which summates the whole numbers up to n:
 |#

;; Type signature: (example-sum int) -> int
(define (example-sum n)
  (if (<= n 0)
      0
      (+ n (example-sum (- n 1)))))



#| If you try to run a recursive function when the base case or recursive call
 |   isn't properly implemented, it may run indefinitely until it runs out of memory.
 | This can freeze up DrRacket, which is annoying but has no permanent consequence.
 | Consider using DrRacket's debug mode if you want to slowly step
 |   through a function to prevent it from crashing.
 |#

;; Run at your own risk! (Nothing actually bad will happen; DrRacket will just seize up)
;; Type signature: (example-bad int) -> an unhappy IDE
(define (example-bad n)
  (cond
    [(<= n 0) 0]
    [(+ n (example-bad (+ n 1)))]))




#| Implement "sum-pow" to accept positive integers p and n
 |   and return X, where X equals the sum of integers to the power p from 1 to n.
 | For example:
 |   If p = 2 and n = 5, then X = 5^2 + 4^2 + 3^2 + 2^2 + 1^2 = 55.
 |   If p = 4 and n = 3, then X = 3^4 + 2^4 + 1^4 = 98.
 | Recommended: use Racket's "expt" function for exponentiation.
 |
 | Examples:
 |   (sum-pow 1 6)  -> 21
 |   (sum-pow 2 15) -> 1240
 |   (sum-pow 5 8)  -> 61776
 |   (sum-pow 10 7) -> 353815700
 |#

;; Type signature: (sum-pow positive-int positive-int) -> int
;; 3 PTS
(define (sum-pow p n)
  (if (<= n 0)
      0
      (+ (expt n p) (sum-pow p (- n 1)))))




#| Implement "subfact" to accept a positive integer n
 |   and return the subfactorial of n (denoted !n).
 | Whereas n! equals the number of permutations of n items,
 |   !n equals the number of derangements of n items.
 | A derangement is a permutation of items where no item is in its initial position.
 | For example, given 5 items in the order [1,2,3,4,5],
 |   [2,4,5,1,3] is a derangement but [5;2;3;1;4] is not.
 |
 | The recursive function S, where S(n) = !n, is defined as follows:
 | S(n) =
 |   0,                if n = 1.
 |   1,                if n = 2.
 |   n * S(n - 1) + 1, if n > 2 and n is even.
 |   n * S(n - 1) - 1, if n > 2 and n is odd.
 |
 | Examples:
 |   (subfact 1)  -> 0
 |   (subfact 3)  -> 2
 |   (subfact 7)  -> 1854
 |   (subfact 15) -> 481066515734
 |
 | Try inputting some larger numbers to see one of the advantages of Racket:
 |   it can compute HUGE numbers really fast!
 |#

;; Type signature: (subfact positive-int) -> int
;; 3 PTS
(define (subfact n)
  (if (= 1 n)
      0
      (if (= 2 n)
          1
          (if
           (integer? (/ n 2))
           (+ (* n (subfact (-  n 1))) 1)
           (- (* n (subfact (-  n 1))) 1)))))




#| Implement "gen-fib", which will compute a generalized version of
 |   the Fibonacci sequence where the two starting values
 |   of the sequence are parameters.
 | The familiar Fibonacci recursive function F is usually defined as follows:
 |   F(n) = 0,                   if n = 0.
 |   F(n) = 1,                   if n = 1.
 |   F(n) = F(n - 1) + F(n - 2), if n > 1.
 | We will generalize the sequence with the following recursive function, G:
 |   G(a,b,n) = a,                           if n = 0.
 |   G(a,b,n) = b,                           if n = 1.
 |   G(a,b,n) = G(a,b,n - 1) + G(a,b,n - 2), if n > 1.
 |
 | (gen-fib a b n) should return G(a,b,n).
 |
 | There are a few different ways to implement gen-fib.
 | One of which is to directly implement the recursive function described above,
 |   which you may do, however it will run slowly for large inputs.
 |
 | If you implement gen-fib such that it returns the correct answer for small inputs,
 |   you will receive the full 3 points for this function.
 | If you implement gen-fib such that it quickly returns the correct answer for small and large inputs,
 |   you will receive an additional 2 points of extra credit on this assignment.
 |
 | Examples for 3 points:
 |   (gen-fib 5 6 0)   -> 5
 |   (gen-fib 5 6 1)   -> 6
 |   (gen-fib 5 6 2)   -> 11
 |   (gen-fib 0 1 8)   -> 21
 |   (gen-fib 0 1 15)  -> 610
 |   (gen-fib 2 1 10)  -> 123
 |   (gen-fib 8 -13 5) -> -41
 |   (gen-fib 0 0 7) -> 0
 |
 | Additional examples for +2 points (each should compute in a second or less):
 |   (gen-fib 0 1 39)    -> 63245986
 |   (gen-fib 2 1 52)    -> 73681302247
 |   (gen-fib 5 6 70)    -> 1730700096559780
 |   (gen-fib 15 -15 80) -> -134165914856871960
 |#

;; Type signature: (gen-fib int int nonnegative-int) -> int
;; 3 PTS (+2 potential PTS)
(define (gen-fib a b n)
  (if (= 0 n)
      a
      (if (= 1 n)
          b
  (+ (gen-fib b (+ a b) (- n 1)))))) 




#|-------------------------------------------------------------------------------|
 |                        Part 3: List Recursion (9 PTS)                         |
 |-------------------------------------------------------------------------------|#

#| Recursion is an essential tool for working with lists in Racket.
 | Here, we'll implement some fundamental recursive functions which operate over lists.
 | Instead of a numerical base case, some of these functions
 |   will have a base case concerning the list, such as the list being empty.
 | For example, below is a recursive function which accepts a nonempty list
 |   and returns the list's last element.
 |#

;; Type signature: (example-list nonempty-list) -> element from list
(define (example-last L)
  (if (null? (cdr L))
      (car L)
      (example-last (cdr L))))




#| Implement "nth" to accept a list L and integer n
 |   and return the element of L at index n.
 | The first element of L is at index 0, the second is at index 1, etc.
 | You may assume that L is not empty, and that 0 ≤ n < length of L. 
 |
 | Examples:
 |   (nth 3 '(Sandeep Franklin Isabella Jared Suzy Toby)) -> Jared
 |   (nth 5 '("zero" "one" "two" "three" "four" "five")) -> "five"
 |   (nth 1 '((a b) (c d) (e f))) -> (c d)
 |#

;; Type signature: (nth int list) -> element from list
;; 2 PTS
(define (nth n L)
  (if (= n 0) (car L) (nth (- n 1) (cdr L))))




#| Implement "product" to accept a list of integers
 |   and returns the result of multiplying said numbers together.
 | Mathematical convention is that the "empty product" - the product of no numbers - is 1.
 | The empty list is a valid input into the function.
 |
 | Examples:
 |   (product '())                     -> 1
 |   (product '(1 3 5 4))              -> 60
 |   (product '(100 -50 6789 4183457)) -> -142007447865000
 |   (product '(6 1 7 245 0 983 -143)) -> 0
 |#

;; Type signature: (product int-list) -> int
;; 3 PTS
(define (product lst)
  (if (= 0 (length lst))
      1
      (* (car lst) (product (cdr lst)))))




#| Implement "filter" to accept a predicate P (a function which returns a boolean) and a list L,
 |   and return a list of all elements in L which satisfy P.
 | In other words, "filter" keeps each element e in L for which P(e) returns true,
 |   and throws out each element e for which P(e) returns false.
 | The resultant list must maintain the original order of the elements in L.
 | You may assume that every element of L is a valid input for P.
 | Even though you can't know what specific function P will be,
 |   you can still invoke it the same as any other function with (P e).
 |
 | Examples (some use lambda functions, check 'em out!):
 |   (filter zero? '(1 0 2 34 56 1 0))
 |     -> (0 0)
 |   (filter even? '(0 1 2 3 4 5 6 7 8 9))
 |     -> (0 2 4 6 8)
 |   (filter number? '(shave and 1 haircut 2 bits))
 |     -> (1 2)
 |   (filter (lambda (s) (= 3 (string-length s))) '("hello" "string" "one" "a" "two" "a b"))
 |     -> ("one" "two" "a b")
 |   (filter (lambda (pair) (car pair)) '((#t 1) (#t 2) (#f 3) (#f 4) (#t 5)))
 |     -> ((#t 1) (#t 2) (#t 5))
 |#

;; Type signature: (filter predicate list) -> list
;; 4 PTS
(define (helpFilter P L lst)
  (cond
    ((null? L) lst)
    ((equal? (P (car L)) #t) (helpFilter P (cdr L) (append lst (list (car L)))))
    (else (helpFilter P (cdr L) lst))))
(define (filter P L)
  (helpFilter P L '()))
