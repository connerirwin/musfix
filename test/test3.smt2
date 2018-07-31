; TODO support polymorphic sorts?
; (qualif Zog ((v a)(z b)) (= v z))
(qualif Zog ((v int)(z int)) (= v z))

(wf $k0 ((v int)(x int)))

; how does the multiple vars format work?
(constraint
  (forall ((v int)(q int))
          (=> (= v q) ($k0 v q))))

(constraint
  (forall ((v int)(y int))
          (=> (&& ($k0 v y)(= y 10)) (= v 10))))
