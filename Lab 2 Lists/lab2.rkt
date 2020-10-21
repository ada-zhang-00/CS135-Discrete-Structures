#lang eopl
#|-------------------------------------------------------------------------------|
 |                            Lab 2: Lists (20 PTS)                              |
 |-------------------------------------------------------------------------------|#

#| This lab serves as an introduction to working with lists in Racket.
 | All lists in Racket are linked lists,
 |   meaning lists are either null [empty],
 |   or a pair of a head [the first element]
 |   and a tail [the rest of the list].
 | This means you can only directly access the first element of a list.
 | To access subsequent elements, you have to repeatedly access tails.
 |
 | Racket has the tick operator ' which is a shorthand for the "quote" function.
 | The tick operator tells the Racket interpreter to treat an expression as a literal
 |   rather than try to evaluate it.
 | By quoting a sequence of things in parentheses, a list is produced.
 | For example, (quote (+ 1 2)), or equivalently '(+ 1 2),
 |   will yield the list (+ 1 2), rather than 3.
 | When you quote a list, it quotes each sub-expression in the list,
 |   allowing you to create nested lists with just one tick mark.
 |#

#| The foundational building block of every list is the empty list,
 |   which is expressed in the following ways (the first is most common):
 |   '()
 |   empty
 |   (list)
 |
 | To construct a list of multiple values, use the "list" function.
 |   (list 1 2 3) -> (1 2 3)
 |   (list (+ 1 2)) -> (3)     <- notice how this differs from (quote (+ 1 2))
 |   (list 'a 'b) -> (a b)     <- '(a b) produces the same result
 |#

#| Here are some useful built-in list functions, where L is a list and E is some element:
 | (null? L) returns #t if L is empty, otherwise #f.
 | (length L) returns the number of elements in L.
 | (reverse L) returns L in reverse order.
 | (cons E L) returns L with E added to the front of the list.
 | (append L1 L2) returns a list of L1 followed by L2.
 | (car L) returns the first element of L. It throws an exception when given an empty list.
 | (cdr L) returns the tail [everything but the first element] of L. It throws an exception when given an empty list.
 |
 | You may not need all of these functions for this lab, but they may be useful in the future.
 |#




#|-------------------------------------------------------------------------------|
 |                     Part 1: List Manipulation (10 PTS)                        |
 |-------------------------------------------------------------------------------|#


#| Implement "name" to accept a first and last name,
 |   and return a list of the first and last name.
 | Example:
 |   (name "Sandeep" "Bhatt") -> ("Sandeep" "Bhatt")
 |#

;; Type signature: (name string string) -> (string string)
;; 1 PTS
(define (name first last)
  (list first last))




#| Implement "last-name" to accept a list of a first and last name
 |   [like those constructed by the "name" function] and return the last name.
 | Example:
 |   (last-name '("Sandeep" "Bhatt"))    -> "Bhatt"
 |   (last-name (name "Jared" "Pincus")) -> "Pincus"
 |#

;; Type signature: (last-name (string string)) -> string
;; 1 PTS
(define (last-name name)
  (cdr name))




#| Implement "yoda" to take a three-part sentence [as a list]
 |   and return the sentence in Yoda-speak.
 | In other words, (w1 w2 w3) becomes (w3 w1 w2).
 | You may assume the input list has exactly 3 elements.
 |
 | Examples:
 |   (yoda '(I love Racket))                  -> (Racket I love)
 |   (yoda '(You are strong-with-the-force))  -> (strong-with-the-force You are)
 |   (yoda '(Sandeep-Bhatt has a-shiny-head)) -> (a-shiny-head Sandeep-Bhatt has)
 |#

;; Type signature: (yoda 3-element-list) -> 3-element-list
;; 2 PTS
(define (yoda words)
  (cons (car (reverse words))
        (reverse( cdr (reverse words)))))
  




#| Implement "pig-latin" to accept a word as a list of characters
 |   and return that word following the rules of Pig Latin.
 | You translate a word into Pig Latin by relocating the first letter
 |   to the end of the word, then adding the suffix "ay".
 | The experienced Pig Latin speakers among you will know that sometimes
 |   you have to move more than one letter from the front of the word to the end,
 |   but here we'll ignore that rule. You merely need to move the first letter.
 | You may assume that the input word is at least one letter long.
 |
 | Examples:
 |   (pig-latin '(h a p p y))       -> (a p p y h a y)
 |   (pig-latin '(b i r t h d a y)) -> (i r t h d a y b a y)
 |   (pig-latin '(t r u e))         -> (r u e t a y)
 |#

;; Type signature: (pig-latin list) -> list
;; 3 PTS
(define (pig-latin word)
  (append(cdr word)
         (cons (car word) '(a y))))




#| Implement "quad-roots" to accept integers a, b, and c,
 |   and return the two roots of the quadratic a*x^2 + b*x + c.
 | As a refresher, the two roots are equal to
 |   (-b ± sqrt(b^2 - 4*a*c)) / (2*a).
 | Format the function's output as a list of
 |   the root produced with minus, followed by
 |   the root produced with plus.
 |
 | While Racket does have support for complex numbers,
 |   you may assume that the roots of the input quadratic
 |   will not include imaginary components.
 | You may also assume that a ≠ 0.
 | Your output for the provided test cases may not exactly match
 |   the provided output. For example, decimals may be represented in fractional form.
 | As long as the values are equivalent mathematically, then your output is correct.
 |
 | Subdefinitions may help to make this function much cleaner!
 |
 | Examples:
 |   (quad-roots -3 0 0)    -> (0 0)
 |   (quad-roots 2 0 -2)    -> (-1 1)
 |   (quad-roots -3 15 -18) -> (3 2)
 |   (quad-roots 8 6 -5)    -> (-1.25 0.5)
 |   (quad-roots 1 -1 -1)   -> (-0.618... 1.618...)
 |   (quad-roots -5 2 2)    -> (0.863... -0.463...)
 |#

;; Type signature: (quad-roots nonzero-int int int) -> (real-number real-number)
;; 3 PTS
(define (quad-roots a b c)
  (list (/(- (- b) (sqrt( - (* b b) (* 4 a c)))) (* 2 a))
        (/(+ (- b) (sqrt( - (* b b) (* 4 a c)))) (* 2 a))))




#|-------------------------------------------------------------------------------|
 |                       Part 2: Nested Lists (10 PTS)                           |
 |-------------------------------------------------------------------------------|#


#| Implement "rotate" to accept an xy-coordinate c
 |   and return a list of the four rotations of c around the origin.
 | These rotations should be in the order 0°, 90° clockwise, 180°, 270° clockwise.
 | As a refresher, the 90° clockwise rotation of
 |   an arbitrary coordinate (x,y) is (y,-x).
 | Consider using subdefinitions to avoid repeatedly accessing the components of c.
 |
 | Examples:
 |   (rotate '(0 1))  -> ((0 1) (1 0) (0 -1) (-1 0))
 |   (rotate '(2 3))  -> ((2 3) (3 -2) (-2 -3) (-3 2))
 |   (rotate '(-5 7)) -> ((-5 7) (7 5) (5 -7) (-7 -5))
 |#
;
;; Type signature: (rotate (int int)) -> ((int int) (int int) (int int) (int int))
;; 2 PTS
(define (helper int num)
  (append (list int) (list (* num (- 0 1)) )))
(define (rotate c)
  (append (append (append (list c) (list (reverse c))) (list (helper (car c) (car(cdr c))))) (list (reverse (helper (car c) (car(cdr c)))))))        

 

#| For the remaining functions in this lab, we'll be working with
 |   an informally defined data structure called a "student".
 | A student is represented by a nested list with the following structure:
 |#
(define student
  '((id-number degree)
    (last-name first-name)
    (birth-day birth-month birth-year)
    (class-year ((major) (minor)) gpa)
    ((number street apt) (city state zip))
    (course1 course1 ... coursen)))


;; Here are two example students for testing your functions:

(define stu1
  '((12345 "Bachelor of Science")
    ("Dongle" "Jonathy")
    (29 "February" 1999)
    (2021 (("Computer Science") ("Math")) 3.75)
    ((5 "Bubble Street" 16) ("Hoboken" "NJ" "07030"))
    ("CS-334" "CS-385" "MA-331" "BT-353")))

(define stu2
  '((10101010101 "Bachelor of Science")
    ("Sprimpling" "Sir Ardlinton")
    (1 "December" 1852)
    (1874 (("Engineering") ("Literature")) 4.000001)
    ((999 "Road Street" 11) ("Old Town Place" "MA" "00001"))
    ("MA-121" "CAL-103" "PE-200" "CH-115" "E-101")))




;; Here's an example function which returns the entire birthday of a student:

(define (get-birthday student)
  (car (cdr (cdr student))))

#| Since the birthday is the third element in the student list, we can access it
 |   by dropping the first two elements (by using cdr twice),
 |   then using car to get the first remaining element.
 |
 | Racket has extra functions for shorthands of nesting car and cdr,
 |   so the body of get-birthday could equivalently be written as (caddr student).
 | A shorthand function exists for every permutation of up to 4 car's and cdr's. Here they all are:
 |    https://docs.racket-lang.org/reference/pairs.html#%28part._.Pair_.Accessor_.Shorthands%29
 |#




;; Now implement the following functions for accessing parts of the student datatype:


#| "get-id-number" returns the id-number field.
 | Examples:
 |   (get-id-number stu1) -> 12345
 |   (get-id-number stu2) -> 10101010101
 |#

;; Type signature: (get-id-number student) -> id-number
;; 1 PTS
(define (get-id-number s)
  (caar s))




#| "get-address" returns the fields which make up the address.
 | Examples:
 |   (get-address stu1) -> ((5 "Bubble Street" 16) ("Hoboken" "NJ" "07030"))
 |   (get-address stu2) -> ((999 "Road Street" 11) ("Old Town Place" "MA" "00001"))
 |#

;; Type signature: (get-address student) -> ((number street apt) (city state zip))
;; 1 PTS
(define (get-address s)
  (car (cddr (cddr s))))




#| "get-gpa" returns the gpa field.
 | Examples:
 |   (get-gpa stu1) -> 3.75
 |   (get-gpa stu2) -> 4.000001
 |#

;; Type signature: (get-gpa student) -> gpa
;; 1 PTS
(define (get-gpa s)
  (car (cddr (car (cdddr s))))) 




#| "get-state" returns the state field.
 | Consider taking advantage of another function you already wrote
 |   for implementing this one!
 | Examples:
 |   (get-state stu1) -> "NJ"
 |   (get-state stu2) -> "MA"
 |#

;; Type signature: (get-state student) -> state
;; 2 PTS
(define (get-state s)
  (car (cdr (car (cdr (get-address s))))))




#| Now implement "combine courses", which accepts two students
 |   and returns a list of the first student's courses
 |   followed by the second student's courses.
 | Consider cleaning this function up with a subdefinition
 |   or external helper function.
 | Examples:
 |   (combine-courses stu1 stu2)
 |     -> ("CS-334" "CS-385" "MA-331" "BT-353" "MA-121" "CAL-103" "PE-200" "CH-115" "E-101")
 |#

;; Type signature: (combine-courses student student) -> string-list
;; 3 PTS
(define (get-course s)
  (car (cddr (cddr (cdr s)))))
(define (combine-courses s1 s2)
  (append (get-course s1) (get-course s2)))
