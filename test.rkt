#lang play

#| Sebastian Mira Pacheco - Sec 2 - 20.8577.485-k|#
(require "T1.rkt")

;; Part B
(test (occurrences p1 "a") 1)
(test (occurrences p2 "a") 3)
(test (occurrences p3 "a") 1)
(test (occurrences p3 "b") 1)
(test (occurrences p3 "c") 1)
(test (occurrences p4 "a") 2)
(test (occurrences p4 "b") 1)
(test (occurrences p5 "a") 1)
(test (occurrences p1 "no hay") 0)

;; Part C
(test (vars p1) (list "a" "b" "c"))
(test (vars p2) (list "a"))
(test (vars p4) (list "a" "b"))

;; Part D

(def l1 (list "a" "b" "c"))
(def l2 (list "a"))
(def l3 (list "a" "b"))

(def l4 (list (list (cons "a" #t) (cons "a" #f)) (list (cons "b" #t) (cons "b" #f)) (list (cons "c" #t) (cons "c" #f))))
(def l5 (list (list (cons "a" #t) (cons "a" #f)) (list (cons "b" #t) (cons "b" #f)) ))
(def l6 (list (list (cons "a" #t) (cons "a" #f))))

; Test for pairs-list
(test (pairs-list l1) (list (list (cons "a" #t) (cons "a" #f)) (list (cons "b" #t) (cons "b" #f)) (list (cons "c" #t) (cons "c" #f))))
(test (pairs-list l2) (list (list (cons "a" #t) (cons "a" #f))))
(test (pairs-list l3) (list (list (cons "a" #t) (cons "a" #f)) (list (cons "b" #t) (cons "b" #f)) ))
(test (pairs-list empty) empty)
; Test for permutation-of-lists
(test (permutation-of-lists l5) ( list ( list (cons "a" #t) (cons "b" #t))
( list (cons "a" #t) (cons "b" #f))
( list (cons "a" #f) (cons "b" #t))
( list (cons "a" #f) (cons "b" #f)))
)
(test (permutation-of-lists l6) ( list
( list (cons "a" #t))
( list (cons "a" #f)))
)
(test (permutation-of-lists empty) (list empty))
; Test for all-enviroments
(test (all-enviroments l2) ( list
( list (cons "a" #t))
( list (cons "a" #f)))
)
(test (all-enviroments l3) ( list ( list (cons "a" #t) (cons "b" #t))
( list (cons "a" #t) (cons "b" #f))
( list (cons "a" #f) (cons "b" #t))
( list (cons "a" #f) (cons "b" #f)))
)
(test (all-enviroments (list "a" "b" "c"))
(list (list (cons "a" #t) (cons "b" #t) (cons "c" #t))
      (list (cons "a" #t) (cons "b" #t) (cons "c" #f))
      (list (cons "a" #t) (cons "b" #f) (cons "c" #t))
      (list (cons "a" #t) (cons "b" #f) (cons "c" #f))
      (list (cons "a" #f) (cons "b" #t) (cons "c" #t))
      (list (cons "a" #f) (cons "b" #t) (cons "c" #f))
      (list (cons "a" #f) (cons "b" #f) (cons "c" #t))
      (list (cons "a" #f) (cons "b" #f) (cons "c" #f))
 )
      )
(test (all-enviroments empty) '(()))

;; Part E
(test (eval (Varp "a") ( list (cons "a" #t))) #t)
(test (eval (Varp "a") ( list (cons "a" #f))) #f)
(test (eval p1 (list (cons "a" #t) (cons "b" #f) (cons "c" #t))) #f)
(test (eval p1 (list (cons "a" #t) (cons "b" #f) (cons "c" #f))) #t)
(test/exn (eval (Varp "a") ( list )) "variable a is not defined in environment")


;; Part F
(def t (Orp (Orp (Varp "a") (Varp "b")) (Orp (Varp "a") (Notp (Varp "b")))))
(def t2 (Orp (Varp "a") (Notp (Varp "a"))))
(def t3 (Orp (Varp "a") (Notp (Varp "b"))))
(test (tautology? t) #t)
(test (tautology? t2) #t)
(test (tautology? t3) #f)

;Exercise 2
;; Part a
(test (simplify-negations (Notp (Varp "a"))) (Notp (Varp "a")))
(test (simplify-negations (Notp (Notp (Varp "a")))) (Varp "a"))
(test (simplify-negations (Notp (Andp (Varp "a") (Varp "b")))) (Orp (Notp (Varp "a")) (Notp (Varp "b"))))
(test (simplify-negations (Notp (Orp (Varp "a") (Varp "b")))) (Andp (Notp (Varp "a")) (Notp (Varp "b"))))
(test (simplify-negations (Notp (Orp (Notp (Varp "a")) (Varp "b")))) (Andp (Notp (Notp (Varp "a"))) (Notp (Varp "b"))))

;; Part b
(test (distribute-and (Andp (Orp (Varp "a") (Varp "b")) (Varp "c"))) (Orp (Andp (Varp "a") (Varp "c")) (Andp (Varp "b") (Varp "c"))))
(test (distribute-and (Andp (Varp "c") (Orp (Varp "a") (Varp "b")))) (Orp (Andp (Varp "c") (Varp "a")) (Andp (Varp "c") (Varp "b"))))
(test (distribute-and (Orp (Andp (Orp (Varp "a") (Varp "b")) (Varp "c")) (Varp "d"))) (Orp (Orp (Andp (Varp "a") (Varp "c")) (Andp (Varp "b") (Varp "c"))) (Varp "d")))
(test (distribute-and (Orp (Andp (Varp "c") (Orp (Varp "a") (Varp "b"))) (Varp "d"))) (Orp (Orp (Andp (Varp "c") (Varp "a")) (Andp (Varp "c") (Varp "b"))) (Varp "d")))

;; Part c
(test (( apply-until (lambda (x) (/ x (add1 x)))(lambda (x new-x) (<= (- x new-x) 0.1))) 1) 0.25)
(test (( apply-until (lambda (x) (- x 1))(lambda (x new-x) (< (+ x new-x) 0))) 10) -1) ;; subtracting 1 until a number plus its predecessor is smaller than 0. Returns the predecessor
(test (( apply-until (lambda (x) (+ x 1))(lambda (x new-x) (> (+ x new-x) 100))) 10) 51) ;; adding 1 until a number plus its successor is greater than 100. Returns the successor

;; Part d
(test (DNF (Andp (Orp (Varp "a") (Varp "b")) (Orp (Varp "c") (Varp "d")))) (Orp
                                                                            (Orp (Andp (Varp "a") (Varp "c"))
                                                                                 (Andp (Varp "a") (Varp "d")))
                                                                            (Orp (Andp (Varp "b") (Varp "c"))
                                                                                 (Andp (Varp "b") (Varp "d")))
                                                                            )
      )

(test (DNF (Notp (Orp (Andp (Notp (Varp "a")) (Varp "b")) (Notp (Varp "d"))))) (Orp (Andp (Varp "a") (Varp "d")) (Andp (Notp (Varp "b")) (Varp "d"))))
(test (DNF (Notp (Orp (Notp (Orp (Notp (Varp "a")) (Varp "b"))) (Notp (Andp (Varp "c") (Notp (Varp "d"))))))) (Orp (Andp (Notp (Varp "a")) (Andp (Varp "c") (Notp (Varp "d")))) (Andp (Varp "b") (Andp (Varp "c") (Notp (Varp "d"))))))
(test (DNF (Varp "a")) (Varp "a"))

; Exercise 3
;; Part a
;; Tests on the next part of the exercise

;; Part b
; occurrences-2
(test (occurrences-2 p1 "a") 1)
(test (occurrences-2 p2 "a") 3)
(test (occurrences-2 p3 "a") 1)
(test (occurrences-2 p3 "b") 1)
(test (occurrences-2 p3 "c") 1)
(test (occurrences-2 p4 "a") 2)
(test (occurrences-2 p4 "b") 1)
(test (occurrences-2 p5 "a") 1)
(test (occurrences-2 p1 "no hay") 0)

; vars-2
(test (vars-2 p1) (list "a" "b" "c"))
(test (vars-2 p2) (list "a"))
(test (vars-2 p4) (list "a" "b"))

; eval-2
(test (eval-2 (Varp "a") ( list (cons "a" #t))) #t)
(test (eval-2 (Varp "a") ( list (cons "a" #f))) #f)
(test (eval-2 p1 (list (cons "a" #t) (cons "b" #f) (cons "c" #t))) #f)
(test (eval-2 p1 (list (cons "a" #t) (cons "b" #f) (cons "c" #f))) #t)
(test/exn (eval (Varp "a") ( list )) "variable a is not defined in environment")

; simplify-negations-2
(test (simplify-negations-2 (Notp (Notp (Varp "a")))) (Varp "a"))
(test (simplify-negations-2 (Notp (Andp (Varp "a") (Varp "b")))) (Orp (Notp (Varp "a")) (Notp (Varp "b"))))
(test (simplify-negations-2 (Notp (Orp (Varp "a") (Varp "b")))) (Andp (Notp (Varp "a")) (Notp (Varp "b"))))
(test (simplify-negations-2 (Notp (Orp (Notp (Varp "a")) (Varp "b")))) (Andp (Notp (Notp (Varp "a"))) (Notp (Varp "b"))))

; distribute-and-2
(test (distribute-and-2 (Andp (Orp (Varp "a") (Varp "b")) (Varp "c"))) (Orp (Andp (Varp "a") (Varp "c")) (Andp (Varp "b") (Varp "c"))))
(test (distribute-and-2 (Andp (Varp "c") (Orp (Varp "a") (Varp "b")))) (Orp (Andp (Varp "c") (Varp "a")) (Andp (Varp "c") (Varp "b"))))

(print-only-errors #t)
