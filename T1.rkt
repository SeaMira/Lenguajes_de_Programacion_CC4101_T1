#lang play
(print-only-errors #t)
#| Sebastian Mira Pacheco - Sec 2 - 20.8577.485-k|#

#| P1 |#


#| Parte A |#
#| 
;; <Prop> ::= ( Varp name)
                | ( Orp <Prop> <Prop> )
                | ( Andp <Prop> <Prop>)
                | ( Notp <Prop>)
|#
;; Inductive type for representing boolean
;; propositions
(deftype Prop
     (Varp name)
     (Orp lprop rprop)
     (Andp lprop rprop)
     (Notp prop)
     )

(def p1 (Orp (Andp (Varp "a") (Varp "b")) (Notp (Varp "c"))))
(def p2 (Orp (Andp (Varp "a") (Varp "a")) (Notp (Varp "a"))))
(def p3 (Orp (Orp (Varp "a") (Varp "b")) (Notp (Varp "c"))))
(def p4 (Andp (Andp (Varp "a") (Varp "a")) (Notp (Varp "b"))))
(def p5 (Notp (Notp (Notp (Varp "a")))))
#| Parte B |#

;; occurrences :: Prop String -> Number
;; given a proposition and a name (string) find
;; how many times variables named like that
;; appear in the proposition

(define (occurrences P N)
    (match P
        ((Varp n) (if (equal? N n) 1 0))
        ((Andp lp rp) (+ (occurrences lp N) (occurrences rp N)))
        ((Orp lp rp) (+ (occurrences lp N) (occurrences rp N)))
        ((Notp p) (occurrences p N))
))


#| Parte C |#

;; vars :: Prop -> (Listof String)
;; Gives a list with all different proposition variables's names in a proposition.

(define (vars P)
  (match P
        ((Varp n) (list n))
        ((Andp lp rp) (remove-duplicates (append (vars lp)(vars rp))))
        ((Orp lp rp) (remove-duplicates (append (vars lp)(vars rp))))
        ((Notp p) (remove-duplicates (vars p)))
        )
  )

#| Parte D |#

;; pairs-list :: (Listof String) -> (Listof (Listof (Pair String Boolean)))
;; Recursive function that creates a list of lists containing pairs of propositions with different boolean evaluation.
(define (pairs-list lst)
  (match lst
    ((list) empty)
    ((cons v h) (append (list (list (cons v #t) (cons v #f))) (pairs-list h)))
   )
  )


;; permutation-of-lists :: (Listof (Listof (Pair String Boolean))) -> (Listof (Listof (Pair String Boolean)))
;; Creates a list with lists representing every possible boolean evaluation enviroment.
(define (permutation-of-lists lst)
  (apply cartesian-product lst)
 )

; apply: in this case it applies a procedure with the contents of a list as args.
; cartesian-product: "multiplies" element-by-element of n lists of same size, obtaining every combination of groups with only one element of every list.


;; all-environments :: (Listof String) -> (Listof (Listof (Pair String Boolean)))
;; Creates every posible combination of proposition-boolean pairs according to different boolean propositions.
(define (all-enviroments lst)
  (permutation-of-lists (pairs-list lst))
 )


#| Parte E |#

;; eval :: Prop (Listof (Pair String Boolean)) -> Boolean
;; Evaluates a boolean proposition using recursion
(define (eval P lst)
  (match P
    ((Varp p) (if (equal? (assoc p lst) #f) (error (string-append "variable " p " is not defined in environment")) (cdr (assoc p lst))))
    ((Orp lp rp) (or (eval lp lst) (eval rp lst)))
    ((Andp lp rp) (and (eval lp lst) (eval rp lst)))
    ((Notp p) (not (eval p lst)))
    )
  )

#| Parte F |#


;; tautology? :: Prop -> Boolean
;; Checks if a proposition is a tautology or not
(define (tautology? P)
  (let
      ((props (vars P)))
    (let ((every-env (all-enviroments props)))
      (foldl (lambda(x y) (and x y)) #t (map (lambda (lst) (eval P lst)) every-env)))
      )
  )



#| P2 |#

#| Parte A |#

;; simplify-negations :: Prop -> Prop
;; Applies a simplification over negations in AND's and OR's distribution with De Morgan laws
(define (simplify-negations Prop)
  (match Prop
      [(Notp (Andp l r)) (Orp (Notp (simplify-negations l)) (Notp (simplify-negations r)))]
      [(Notp (Orp l r)) (Andp (Notp (simplify-negations l)) (Notp (simplify-negations r)))]
      [(Notp (Notp p)) (simplify-negations p)]
      [(Notp (Varp p)) (Notp (Varp p))]
      [(Andp l r) (Andp (simplify-negations l) (simplify-negations r))]
      [(Orp l r) (Orp (simplify-negations l) (simplify-negations r))]
      [(Varp p) (Varp p)]
      )
  )


#| Parte B |#

;; distribute-and :: Prop -> Prop
;; Applies AND distribution over a proposition following De Morgan's laws
(define (distribute-and Prop)
  (match Prop
    [(Andp (Orp l r) p) (Orp (Andp l p) (Andp r p))]
    [(Andp p (Orp l r)) (Orp (Andp p l) (Andp p r))]
    [(Andp l r) #:when (and (not (Orp? l)) (not (Orp? r))) (Andp (distribute-and l) (distribute-and r))]
    [(Orp l r) (Orp (distribute-and l) (distribute-and r))]
    [(Notp p) (Notp (distribute-and p))]
    [(Varp p) (Varp p)]
    )
  )

#| Parte C |#

;; apply-until :: (a -> a) (a a -> Boolean) -> a -> a
;; Returns a function that applies another function f to an argument until a predicate p
;; returns #t.
(define (apply-until f p)
  (letrec ([fun (lambda (x) (if (p x (f x)) (f x) (fun (f x))))]) fun)
 )

#| Parte D |#

;; DNF :: Prop -> Prop
;; Applies distribute-and and simplify-negations until Prop reaches its Disjunctive Normal Form
(define (DNF Prop)
  (let
      (
       [SN-DA (lambda (x) (simplify-negations (distribute-and x)))]
       )
      ((apply-until SN-DA equal?) Prop)
      )
  )



#| P3 |#

#| Parte A |#

;; fold-prop :: (String -> a) (a a -> a) (a a -> a) (a -> a) -> Prop -> a
;; Captures the recursion scheme associated to Prop, running a specific function for every symbol on its grammar
(define (fold-prop varf andf orf notf)
  (lambda (Prop)
    (match Prop
      [(Varp p) (varf p)]
      [(Andp l r) (andf ((fold-prop varf andf orf notf) l) ((fold-prop varf andf orf notf) r))]
      [(Orp l r) (orf ((fold-prop varf andf orf notf) l) ((fold-prop varf andf orf notf) r))]
      [(Notp p) (notf ((fold-prop varf andf orf notf) p))]
      )
    )
  )


#| Parte B |#

;; occurrences-2 :: Prop String -> Number
;; Given a Proposition and a string, returns the number of appearances of that string as proposition variable's names
(define (occurrences-2 Prop N)
  (let ([fun
         (fold-prop
          (lambda (x) (if (equal? x N) 1 0))
          (lambda (x y) (+ x y))
          (lambda (x y) (+ x y))
          (lambda (x) x))]) (fun Prop))
  )

;; vars-2 :: Prop -> (Listof String)
;; Given a Proposition returns a list with every different proposition variable name
(define (vars-2 Prop)
  (let ([fun
         (fold-prop
          (lambda (x) (list x))
          (lambda (x y) (remove-duplicates (append x y)))
          (lambda (x y) (remove-duplicates (append x y)))
          (lambda (x) x))]) (fun Prop))
  )

;; eval-2 :: Prop (Listof (Pair String Boolean)) -> Boolean
;; Given a Proposition and a list of pairs of proposition variables names with their truth values, tells if the proposition is True or False
(define (eval-2 Prop lst)
  (let ([fun
         (fold-prop
          (lambda (x) (if (equal? (assoc x lst) #f) (error (string-append "variable " x " is not defined in environment")) (cdr (assoc x lst))))
          (lambda (x y) (and x y))
          (lambda (x y) (or x y))
          (lambda (x) (not x)))]) (fun Prop))
  )

;; simplify-negations-2 :: Prop -> Prop
;; Given a proposition, simplifies the "not" operations if possible on inner propositions (according to De Morgan laws) and returns a simplified proposition
(define (simplify-negations-2 Prop)
  (let ([fun
         (fold-prop
          (lambda (x) (Varp x))
          (lambda (x y) (Andp x y))
          (lambda (x y) (Orp x y))
          (lambda (x) (match x
                        [(Orp l r) (Andp (Notp l) (Notp r))]
                        [(Andp l r) (Orp (Notp l) (Notp r))]
                        [(Notp p) p]
                        [(Varp p) (Notp (Varp p))]
                       )))]) (fun Prop))
  )

;; distribute-and-2 :: Prop -> Prop
;; Given a proposition, function ditributes "and" operations over "or" operations on inner propositions (according to De Morgan laws), returning a simplified Proposition
(define (distribute-and-2 Prop)
  (let ([fun
         (fold-prop
          (lambda (x) (Varp x))
          (lambda (x y) (cond
                          [(Orp? x) (match x [(Orp l r) (Orp (Andp l y) (Andp r y))])]
                          [(Orp? y) (match y [(Orp l r) (Orp (Andp x l) (Andp x r))])]
                          [else (Andp x y)]
                         ))
          (lambda (x y) (Orp x y))
          (lambda (x) (Notp x)))]) (fun Prop))
  )
