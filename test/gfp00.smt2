(qualif Pos   ((v Int)) (<= 0 v))
(qualif Neg   ((v Int)) (<= v 0))
(qualif NeqZ  ((v Int)) (not (= v 0)))
(qualif False ()        (= 66 77))

(wf $k0 ((v0 Int)))

(constraint
  (forall ((v1 Int))
          (=> ($k0 v1) (< 0 (+ v1 1)))))

(constraint
  (forall ((v2 Int))
          (=> (= v2 10) ($k0 v2))))
