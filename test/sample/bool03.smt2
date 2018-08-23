; Sorts
(declare-sort Str 0)

; Functions
(declare-const cat Str)
(declare-const not_the_hippopotamus Str)
(declare-const dog Str)

; Qualifiers
; (qualif LE ((v @a0)(x @a0)) (<= x v))
; I have no idea how you are supposed to infer this, but casting bools to int
; and then performing <= is the same as checking implies
(qualif Implies ((v Bool) (x Bool)) (=> x v))

; Well-formedness constraints
(wf $k1 ((bx Bool)(v Bool)))

; Horn constraints
(constraint (forall ((bx Bool) (v1 Bool)) (=> (and True (=> bx v1)) ($k1 bx v1))))
(constraint (forall ((bx Bool) (zx Bool) (v2 Bool)) (=> (and True (and (not (= cat dog)) ($k1 bx v2))) (=> bx v2))))
