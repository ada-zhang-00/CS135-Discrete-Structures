#lang eopl

#|-------------------------------------------------------------------------------|
 |                  Lab 6: Relations Part II (20 PTS +3 EC)                      |
 |-------------------------------------------------------------------------------|#

#| This lab is a continuation of the previous lab about relations.
 | All the functions you wrote Part I are now available
 |   as helper functions at the bottom of this file,
 |   along with the same helper functions as in Part I.
 | Check what helper functions are available before starting
 |   so you can best take advantage of them.
 |
 | The specifications of this lab are the same as in Part I
 |   regarding how relations are defined and graded.
 |
 | For the provided test cases which output large relations,
 |   consider comparing your output to the expected output
 |   by using "set-equal?".
 | As usual, always try out your own test cases,
 |   as the provided ones may not be comprehensive.
 | The empty relation is always valid input for a relation!
 |#




#| Implement "compose" to accept two relations S and R,
 |   and return S ∘ R, or S composed with R.
 | S ∘ R = { (x z) | ∃y: (x y) ∈ R ∧ (y z) ∈ S }.
 |
 | Advice for implementation:
 |   Consider each edge (a b) in R. For every edge (c d) in S,
 |   if b and c are equal then add (a d) to the output.
 |   See if you can leverage the "relate" helper function!
 |
 | Examples:
 |   (compose '() '((3 2) (2 4)) )
 |     -> ()
 |   (compose '((1 1) (2 1) (1 3)) '() )
 |     -> ()
 |   (compose '((2 4) (3 5) (4 6) (5 7)) '((1 2) (2 3) (3 4) (4 5)) )
 |     -> ((1 4) (2 5) (3 6) (4 7))
 |   (compose '((2 4) (5 8) (3 4)) '((2 4) (5 8) (3 4)) )
 |     -> ()
 |   (compose '((1 1) (2 2) (3 2)) '((1 1) (2 2) (2 3)) )
 |     -> ((1 1) (2 2))
 |   (compose '((1 1) (2 1) (3 1) (4 1)) '((1 1) (1 2) (1 3)) )
 |     -> ((1 1))
 |   (compose '((1 1) (1 2) (1 3)) '((1 1) (2 1) (3 1) (4 1)) )
 |     -> ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3) (4 1) (4 2) (4 3))
 |#

;; Type Signature: (compose relation relation) -> relation
;; 5 PTS
(define (compose S R)
  (if (= (length S) 0)
       (list)
       (append (composeHelper (car S) R) (compose (cdr S) R))))

(define (composeHelper E R)
  (if (= (length R) 0)
      (list)
      (if (equal? (car (cdr (car R))) (car E))
          (cons (list (car (car R)) (car (cdr E))) (composeHelper E (cdr R)))
          (composeHelper E (cdr R)))))


#| Implement "power" to accept an integer k ≥ 0 and a relation R
 |   and return R^k, which equals R composed with itself (k - 1) times.
 |
 | R^0 = ∅
 | R^1 = R
 | R^2 = R ∘ R
 | R^3 = R ∘ (R ∘ R)
 | ...
 | R^k = R ∘ (R^(k-1))
 |
 | Advice:
 |   You'll need to keep track of the initial R to repeatedly compose it.
 |   To do so, you'll likely need a helper function.
 |
 | Examples:
 |   (power 0 '((1 2) (2 3) (3 4) (4 1)) )
 |     -> ()
 |   (power 3 '() )
 |     -> ()
 |   (power 2 '((1 2) (2 3) (3 4) (4 1)) )
 |     -> ((1 3) (2 4) (3 1) (4 2))
 |   (power 4 '((1 2) (3 3) (3 4) (4 2) (5 3)) )
 |     -> ((3 3) (3 4) (3 2) (5 3) (5 4) (5 2))
 |   (power 1 '((1 2) (2 3) (4 3) (2 4) (5 6)) )
 |     -> ((1 2) (2 3) (4 3) (2 4) (5 6))
 |   (power 2 '((1 2) (2 3) (4 3) (2 4) (5 6)) )
 |     -> ((1 3) (1 4) (2 3))
 |   (power 3 '((1 2) (2 3) (4 3) (2 4) (5 6)) )
 |     -> ((1 3))
 |   (power 4 '((1 2) (2 3) (4 3) (2 4) (5 6)) )
 |     -> ()
 |#

;; Type Signature: (power nonnegative-int relation) -> relation
;; 5 PTS

(define (power k R)
    (if (= k 0)
      (list)
      (if (= k 1)
          R
          (powerHelp R (power (- k 1) R)))))


(define (powerHelp R R1)
  (compose R R1))


#| Implement "transitive-closure", which accepts a relation R
 |   and returns R+, the transitive closure of R.
 | R+ can be computed by "unioning" successive powers of R.
 | If e is the number of edges in R [a.k.a. the cardinality of R]:
 |   R+ = R^0 ∪ R^1 ∪ R^2 ∪ R^3 ∪ ... ∪ R^e.
 | Again, you'll probably want a helper function!
 |
 | Examples:
 |   (transitive-closure '() )
 |     -> ()
 |   (transitive-closure '((4 4)) )
 |     -> ((4 4))
 |   (transitive-closure '((1 2) (3 4)) )
 |     -> ((1 2) (3 4))
 |   (transitive-closure '((1 2) (2 3) (3 1)) )
 |     -> ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
 |   (transitive-closure '((1 3) (3 5) (2 4) (5 6) (2 3)) )
 |     -> ((1 3) (3 5) (2 4) (5 6) (2 3) (1 5) (1 6) (3 6) (2 5) (2 6))
 |   (transitive-closure '((1 2) (2 1) (3 4) (4 5)) )
 |     -> ((1 1) (2 2) (3 5) (1 2) (2 1) (3 4) (4 5))
 |#

;; Type Signature: (transitive-closure relation) -> relation
;; 5 PTS
(define (transitive-closure R)
  (helper R (cardinality R)))

(define (helper R p)
  (if (equal? p 0)
      ' ()
  (union (power p R) (helper R (- p 1)))))






#| Implement "transitive?" to which accepts a relation R
 |   and return whether R is transitive.
 | This should be very similar to how "reflexive?" and "symmetric?" are implemented.
 |
 | Examples:
 |   (transitive? '() )
 |     -> #t
 |   (transitive? '((3 3)) )
 |     -> #t
 |   (transitive? '((1 1) (2 1)) )
 |     -> #t
 |   (transitive? '((5 3) (3 6) (6 5)) )
 |     -> #f
 |   (transitive? '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) )
 |     -> #t
 |   (transitive? '((1 2) (2 3) (3 1)) )
 |     -> #f
 |   (transitive? '((1 2) (2 1) (1 1) (2 2)) )
 |     -> #t
 |#

;; Type signature: (transitive? relation) -> boolean
;; 3 PTS
(define (transitive? R)
  (set-equal? (transitive-closure R) R))


#| Implement "equivalence?" to accept a positive integer n and a relation R
 |   and return whether R is an equivalence relation over [1, n].
 | Recall that R is an equivalence relation
 |   if it is reflexive over [1, n], symmetric, and transitive.
 | 
 | Examples:
 |   (equivalence? 1 '() )
 |     -> #f
 |   (equivalence? 2 '((2 2)) )
 |     -> #f
 |   (equivalence? 3 '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) )
 |     -> #t
 |   (equivalence? 2 '((1 1) (1 2) (2 1)) )
 |     -> #f
 |   (equivalence? 4 '((1 1) (2 2) (3 3) (4 4) (3 4) (4 3)) )
 |     -> #t
 |   (equivalence? 1 '((1 1) (2 3) (3 2)) )
 |     -> #f    [Do you see why this is false despite being reflexive up to 1?]
 |   (equivalence? 1 '((1 1) (2 4) (4 2) (2 2) (4 4)) )
 |     -> #t
 |   (equivalence? 4 '((1 1) (2 4) (4 2) (2 2) (4 4)) )
 |     -> #f    [In comparison to the previous example, this fails because (3 3) is missing]
 |#

;; Type signature: (equivalence? positive-int relation) -> boolean
;; 2 PTS
(define (equivalence? n R)
  (if (transitive? R)
      (if (symmetric? R)
          (if (reflexive? n R)
              #t
              #f)
          #f)
      #f))




#| OPTIONAL:
 | Implement "involutive?" to accept a positive int n and relation R
 |   and return whether R is an involution over [1, n].
 | An involution, or involutive function, is a function
 |   which equals its own inverse.
 | In other words, composing the function with itself yields the identity function.
 | For a relation to be involutive, it must be bijective.
 |
 | So in summary, R is involutive if it is
 |   injective over [1, n], surjective over [1, n], and symmetric.
 |
 | R is surjective over [1, n] if ∀y ∈ [1, n]: ∃x : (x y) ∈ R.
 | Make sure to check the provided helper functions to make this easier!
 |
 | Some well-known examples of involutions include:
 |   Negation:     p = ¬(¬p)
 |   Exclusive OR: p = q ⨁ (q ⨁ p)
 |   Reciprocal:   x = 1 / (1 / x)
 |
 | Examples:
 |   (involutive? 2 '((1 2) (2 1)) )
 |     -> #t    [This relation imitates the negation function.]
 |   (involutive? 3 '((3 3) (1 1) (2 2)) )
 |     -> #t    [The identity function is the simplest involution...]
 |   (involutive? 5 '((3 3) (1 1) (2 2) (5 5)) )
 |     -> #f    [... but it's not involutive if some of the domain is missing.]
 |   (involutive? 4 '((1 2) (2 3) (3 4) (4 1)) )
 |     -> #f    [The cyclic relation up to 4 isn't involutive...]
 |   (involutive? 4 (power 2 '((1 2) (2 3) (3 4) (4 1)) ))
 |     -> #t    [... but composing it with itself produces an involution!]
 |   (involutive? 3 '((1 5) (2 3) (3 4) (4 2)) )
 |     -> #f    [A function with a different domain and range can't be involutive.]
 |   (involutive? 3 '((1 2) (2 2) (3 3) (4 1)) )
 |     -> #f    [A non-bijective function can't be involutive either.]
 |#

;; Type signature: (involutive? positive-int relation) -> boolean
;; 3 Extra Credit PTS
(define (involutive? n R)
  "TODO: Implement")




#|-------------------------------------------------------------------------------|
 |                           Relation Helper Functions                           |
 |-------------------------------------------------------------------------------|#


;; "id" accepts a positive integer n and returns the smallest reflexive relation over [1, n].
;; Type signature: (id positive-int) -> relation
(define (id n)
  (if (zero? n) '()
      (cons (list n n)
            (id (- n 1)))))


;; "reflexive?" accepts a positive integer n and relation R and returns whether R is reflexive.
;; Type signature: (reflexive? positive-int relation) -> boolean
(define (reflexive? n R)
  (subset? (id n) R))


;; "reflexive-closure" accepts a positive integer n and relation R
;;   and returns the reflexive closure of R over [1, n].
;; Type signature: (reflexive-closure positive-int relation) -> relation
(define (reflexive-closure n R)
  (union R (id n)))


;; "converse" accepts a relation R and returns { (y x) | (x y) ∈ R }.
;; Type signature: (converse relation) -> relation
(define (converse R)
  (map reverse R))


;; "symmetric?" accepts a relation R and returns whether R is symmetric.
;; Type signature: (symmetric? relation) -> boolean
(define (symmetric? R)
  (set-equal? R (converse R)))


;; "symmetric-closure" accepts a relation R and returns the symmetric closure of R.
;; Type signature: (symmetric-closure relation) -> relation
(define (symmetric-closure R)
  (union R (converse R)))


;; "relate" accepts a positive integer x and relation R and returns { y | (x y) ∈ R }.
;; Type signature: (relate positive-int relation) -> set-of-ints
(define (relate x R)
  (define (relate-helper x R)
    (cond
      [(null? R) '()]
      [(= x (caar R))
       (cons (cadar R)
             (relate-helper x (cdr R)))]
      [else (relate-helper x (cdr R))]))
  (make-set (relate-helper x R)))


;; "injective?" accepts a relation R and returns whether R is injective.
;; Type signature: (injective? relation) -> boolean
(define (injective? R)
  (define range (map car (converse R)))
  (= (length range)
     (cardinality range)))


#|-------------------------------------------------------------------------------|
 |                             Set Helper Functions                              |
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


;; "insert" accepts a set S and an element e, and returns {e} ∪ S.
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
