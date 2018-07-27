; TODO support polymorphic sorts?
; (qualif Zog ((v a)(z b)) (= v z))
(qualif Zog ((v int)(z int)) (= v z))

(wf $k0 ((v int)))

(constraint
  (forall ((v int)(q int))
          (=> (= v q) ($k0 q))))

(constraint
  (forall ((v int)(y int))
          (=> (&& (= y 42)($k0 y)) (= v 10))))
