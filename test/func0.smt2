; Parsing of uniterpreted functions
(qualif Eq ((v @a)(z @b)) (= v z))

(declare-fun bar (Int) Bool)

(wf $k0 ((v0 Int)(v1 Int)))

; Various Errors
; (qualif QQ ((b Bool)) (= (bar b) True))
; (qualif QQ ((i Int)) (= (bar i) 1))

(constraint
  (forall ((v2 Int)(v3 Int))
          (=> ($k0 v2 v3) (= (bar v2)(bar v3)))))
