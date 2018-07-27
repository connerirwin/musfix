(qualif Zog ((v a)(z b)) (= v z))

(wf $k0 ((v int)))

(constraint
  (forall ((v int)(q int))
          (=> (= v q) ($k0 q))))

(constraint
  (forall ((v int))
          (=> ($k0 10) (= v 10))))
