#lang eopl
#|-------------------------------------------------------------------------------|
 |                      Lab 5: Relations Part I (20 PTS)                         |
 |-------------------------------------------------------------------------------|#

#| In this lab, we'll write functions which operate over relations.
 | A relation is a set of pairs, so as a continuation of the
 |   previous lab's representation of sets,
 |   we'll represent a relation as a set of pairs of integers.
 | (NOTE: references to the term "pair" in the lab refer to ordered lists
 |    of two integers, NOT the Racket data structure called a "pair").
 | The pair (x y) with integers x and y represents
 |   an "arrow" pointing from x to y in the relation.
 |
 | Every relation in this lab will have a subset of the positive integers as its domain,
 |   which will be notated in the instructions as [1, n] where n is an integer ≥ 1.
 | [1, n] represents the domain of values: 1, 2, ..., n.
 |
 | Like in the previous lab, we'll use lists to represent sets,
 |   but we won't let there be duplicate elements and
 |   we won't care about the elements' order.
 | If the output type of a function is a "relation",
 |   the order of the pairs in the output list doesn't
 |   need to match the test cases' output order to be correct.
 |
 | If the type of a function's input is a "relation",
 |   you may assume that the relation is correctly formed;
 |   that is, the relation only contains unique pairs of integers.
 |
 | *** The empty relation is always valid input for relation inputs! ***
 |
 | At the bottom of this file, you'll find many helper functions,
 |   all from the previous lab, which you may or may not need to help you complete this one.
 | Since a relation is a type of set, the helpers which operate on sets will come in handy!
 |
 | Also, an important note:
 |   If you want to skip a function, whether it's extra credit or otherwise,
 |   please don't comment out the function's declaration. Instead,
 |   keep the function there and have it return something useless,
 |   like the "TODO" strings which are in the function bodies by default.
 |
 | From this point forward with these lab assignments,
 |   you should be comfortable using recursion, lists, if/cond expressions,
 |   and sometimes writing subdefinitions/helper functions.
 | If you want to take the extra step to write more advanced/efficient code,
 |   let bindings and lambda functions are two highly useful
 |   but completely optional constructs worth learning.
 |#




#| Implement "id" to accept a positive integer
 |   and return the "identity relation" over [1, n].
 | This means it should create a set
 |   containing the pairs (1 1), (2 2), ..., (n n).
 |
 | Examples:
 |   (id 1) -> ((1 1))
 |   (id 2) -> ((1 1) (2 2))
 |   (id 5) -> ((1 1) (2 2) (3 3) (4 4) (5 5))
 |#

;; Type signature: (id positive-int) -> relation
;; 3 PTS
(define (id n)
  (cond
    [(= 0 n) '()]
    [else (cons (list n n) (id(- n 1)))]))




#| Implement "reflexive?" to accept a positive integer n and relation R,
 |   and return whether R is reflexive over the domain [1, n].
 | In other words, R is reflexive if it contains the identity relation up to n.
 | This can be implemented easily using "id" and one of the provided helper functions.
 |
 | Examples:
 |   (reflexive? 3 '())                              -> #f
 |   (reflexive? 2 '((3 3) (1 1) (2 2)))             -> #t
 |   (reflexive? 3 '((2 3) (1 1) (3 3) (3 4) (2 2))) -> #t
 |   (reflexive? 4 '((1 1) (2 2) (3 3)))             -> #f
 |   (reflexive? 1 '((1 4) (2 3) (3 1) (3 3) (4 4))) -> #f
 |   (reflexive? 2 '((4 3) (2 2) (1 3) (1 1)))       -> #t
 |#

;; Type signature: (reflexive? positive-int relation) -> boolean
;; 2 PTS
(define (reflexive? n R)
  (cond
    [(subset? (id n) R) #t]
    [else #f]))




#| Implement "reflexive-closure" to accept a positive integer n and a relation R,
 |   and return the reflexive closure of R over [1, n],
 |   which is the smallest relation that contains R and is reflexive over [1, n].
 | This can also be implemented easily using "id" and one of the provided helper functions.
 |
 | Examples:
 |   (reflexive-closure 3 '())                  -> ((1 1) (2 2) (3 3))
 |   (reflexive-closure 3 '((3 2) (2 3)))       -> ((1 1) (2 2) (3 3) (3 2) (2 3))
 |   (reflexive-closure 4 '((1 1) (2 2) (3 3))) -> ((1 1) (2 2) (3 3) (4 4))
 |   (reflexive-closure 1 '((1 1) (2 2) (3 3))) -> ((1 1) (2 2) (3 3))
 |   (reflexive-closure 2 '((1 3) (3 2) (3 3))) -> ((1 3) (3 2) (3 3) (1 1) (2 2))
 |   (reflexive-closure 1 '((2 4) (5 7) (4 3))) -> ((2 4) (5 7) (4 3) (1 1))
 |#

;; Type signature: (reflexive-closure positive-int relation) -> relation
;; 2 PTS
(define (reflexive-closure n R)
  (make-set (append R (id n))))




#| Implement "converse" to accept a relation R, and return the converse of R.
 | We can define R's converse in set-builder notation as { (y, x) | (x, y) ∈ R }.
 | In other words, the converse of R is the same as R but the inputs and outputs are flipped.
 | Hint: consider Racket's "map" and "reverse" functions for a really easy way to implement this!
 |
 | Examples:
 |   (converse '())                  -> ()
 |   (converse '((1 2) (3 2) (4 5))) -> ((2 1) (2 3) (5 4))
 |   (converse '((1 1) (1 2) (1 3))) -> ((1 1) (2 1) (3 1))
 |   (converse '((2 6) (4 3)))       -> ((6 2) (3 4))
 |#

;; Type signature: (converse relation) -> relation
;; 3 PTS
(define (converse R)
  (map reverse R))




#| Implement "symmetric?", which accepts a relation R and returns whether R is symmetric.
 | R is symmetric iff ∀(x, y) ∈ R : (y, x) ∈ R.
 | This can be implemented easily using "converse" and one of the provided helper functions.
 |
 | Examples:
 |   (symmetric? '())                              -> #t
 |   (symmetric? '((1 1) (2 1) (1 2)))             -> #t
 |   (symmetric? '((1 1) (2 4) (3 7) (3 5) (5 3))) -> #f
 |   (symmetric? '((2 4) (4 3) (3 4) (4 2)))       -> #t
 |   (symmetric? '((1 1) (4 4)))                   -> #t
 |   (symmetric? '((1 2) (2 1) (3 3) (2 3)))       -> #f
 |#

;; Type signature: (symmetric? relation) -> boolean
;; 2 PTS
(define (symmetric? R)
  (subset? (converse R) R))




#| Implement "symmetric-closure" to accept a relation R and return the symmetric closure of R,
 |   which is the smallest relation which is symmetric and contains R.
 | This can be implemented easily using "converse" and one of the provided helper functions.
 |
 | Examples:
 |   (symmetric-closure '())                  -> ()
 |   (symmetric-closure '((3 2) (2 3)))       -> ((3 2) (2 3))
 |   (symmetric-closure '((1 2) (2 7) (3 4))) -> ((1 2) (2 7) (3 4) (2 1) (7 2) (4 3))
 |   (symmetric-closure '((1 1) (2 2) (1 3))) -> ((1 1) (2 2) (1 3) (3 1))
 |   (symmetric-closure '((3 3)))             -> ((3 3))
 |#

;; Type signature: (reflexive-closure relation) -> relation
;; 2 PTS
(define (symmetric-closure R)
  (make-set (append R (converse R))))




#| Implement "relate", which accepts a relation R and a positive integer x
 |   and returns the set of values to which x relates through R.
 | We represent the output of (relate x R) in set-builder notation as { y | (x, y) ∈ R }.
 |
 | Examples:
 |   (relate 1 '())                        -> ()
 |   (relate 3 '((3 3) (3 4) (3 5)))       -> (3 4 5)
 |   (relate 1 '((1 2) (2 3) (3 4) (4 5))) -> (2)
 |   (relate 2 '((1 3) (3 2) (4 6) (8 2))) -> ()
 |   (relate 2 '((1 2) (3 2) (2 2) (2 4))) -> (2 4)
 |#

;; Type signature: (relate positive-int relation) -> set-of-ints
;; 3 PTS
(define (relate x R)
  (cond [(null? R) '()]
        [(equal? x (car (car R))) (append (cdr (car R)) (relate x (cdr R)))]
        [else (relate x (cdr R))]))




#| Implement "injective?" to accept a relation R and return whether R is injective.
 | R is injective if every value in R's range is related to at most one value in R's domain.
 |
 | Examples:
 |   (injective? '())                  -> #t
 |   (injective? '((1 2) (1 3) (3 5))) -> #t
 |   (injective? '((4 1) (3 1)))       -> #f
 |   (injective? '((2 2) (3 1) (1 2))) -> #f
 |   (injective? '((1 2) (2 3) (3 4))) -> #t
 |#

;; Type signature: (injective? relation) -> boolean
;; 3 PTS
(define (injective? R)
  "TODO IMPLEMENT")


  

#|-------------------------------------------------------------------------------|
 |              Helper Functions: use them to your heart's content!              |
 |-------------------------------------------------------------------------------|#

;; "make-set" accepts a list and returns the list without duplicate elements.
;; Type signature: (make-set list) -> set
(define (make-set L)
  (cond [(null? L) '()]
        [(member (car L) (cdr L)) (make-set (cdr L))]
        [else (cons (car L) (make-set (cdr L)))]))


;; "element?" accepts a set S and an element e and returns #t iff e ∈ S.
;; Type signature: (element? set any) -> boolean
(define (element? S e)
  (cond [(null? S) #f]
        [(equal? e (car S)) #t]
        [else (element? (cdr S) e)]))


;; "insert" accepts a set S and an element e, and returns {e} ⋃ S.
;; Type signature: (insert set any) -> set
(define (insert S e)
  (make-set (cons e S)))


;; "remove" accepts a set S and element e and returns S – {e}.
;; Type signature: (remove set any) -> set
(define (remove S e)
  (cond [(null? S) S]
        [(equal? e (car S)) (cdr S)]
        [else (cons (car S) (remove (cdr S) e))]))


;; "cardinality" accepts a list L and returns the number of unique elements in L.
;; Type signature: (cardinality list) -> int
(define (cardinality L)
  (length (make-set L)))


;; "powerset" accepts a set S and returns the set { T | T ⊆ S }.
;; Type signature: (powerset set) -> set-of-sets
(define (powerset S)
  (if (null? S) '(())
      (let ([r (powerset (cdr S))])
        (append r (map (lambda (s) (cons (car S) s))
                       r)))))


;; "union" accepts sets S1 and S2 and returns S1 ⋃ S2.
;; Type signature: (union set set) -> set
(define (union S1 S2)
  (make-set (append S1 S2)))


;; "intersection" accepts sets S1 and S2 and returns S1 ⋂ S2.
;; Type signature: (intersection set set) -> set
(define (intersection S1 S2)
  (define (helper S1 S2 acc)
    (if (null? S1) acc
        (helper (cdr S1) S2
                (if (element? S2 (car S1))
                    (cons (car S1) acc)
                    acc))))
  (helper S1 S2 '()))


;; "disjoint?" accepts sets S1 and S2 and returns whether S1 ⋂ S2 = ∅.
;; Type signature: (disjoint? set set) -> boolean
(define (disjoint? S1 S2)
  (null? (intersection S1 S2)))


;; "subset?" accepts sets S1 and S2 and returns S1 ⊆ S2.
;; Type signature: (subset? set set) -> boolean
(define (subset? S1 S2)
  (cond [(null? S1) #t]
        [(element? S2 (car S1))
         (subset? (cdr S1) S2)]
        [else #f]))


;; "set-equal?" accepts sets S1 and S2 and returns S1 = S2.
;; Type signature: (set-equal? set set) -> boolean
(define (set-equal? S1 S2)
  (and (subset? S1 S2)
       (subset? S2 S1)))


;; "difference" accepts sets S1 and S2 and returns S1 – S2.
;; Type signature: (difference set set) -> set
(define (difference S1 S2)
  (if (null? S2) S1
      (difference (remove S1 (car S2))
                  (cdr S2))))


;; "sym-diff" to accept sets S1 and S2 and returns S1 ⊖ S2 (symmetric difference).
;; Type signature: (sym-diff list list) -> set
(define (sym-diff S1 S2)
  (union (difference S1 S2)
         (difference S2 S1)))
