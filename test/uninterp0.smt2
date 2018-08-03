; Parsing of uniterpreted functions
(qualif Eq ((v @a)(z @b)) (= v z))

(define-fun bar (Int) Bool)
; (define-fun len (Int Int) Int)
; (define-fun foo (Bool Int) Int)

(wf $k0 ((v0 Int)(v1 Int)))

(constraint
  (forall ((v2 Int)(v3 Int))
          (=> ($k0 v2 v3) (= (bar v2)(bar v3)))))
