; TODO support polymorphic sorts?
; (qualif Zog ((v a)(z b)) (= v z))
(qualif Zog ((v Int)(z Int)) (= v z))

(wf $k0 ((v Int)(x Int)))

; how does the multiple vars format work?
(constraint
  (forall ((v Int)(q Int))
          (=> (= v q) ($k0 v q))))

(constraint
  (forall ((v Int)(y Int))
          (=> (&& ($k0 v y)(= y 10)) (= v 10))))
