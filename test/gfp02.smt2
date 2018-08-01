(qualif Pos   ((v Int)) (<= 0 v))
(qualif Neg   ((v Int)) (<= v 0))
(qualif NeqZ  ((v Int)) (not (= v 0)))
(qualif GtTen ((v Int)) (< v 10))
(qualif False ()        (= 66 77))

(wf $k0 ((v0 Int)))

(constraint
  (forall ((v1 Int))
          (=> ($k0 v1) (< 0 (+ v1 1)))))  ; 0 < (v1 + 1)

(constraint
  (forall ((v3 Int))
          (=> ($k0 v3) (> 11 (+ v3 1))))) ; 11 > (v3 + 1)

(constraint
  (forall ((v2 Int))
          (=> (= v2 9) ($k0 v2))))

(constraint
  (forall ((v4 Int))
        (=> (&& (< v4 9) (> v4 3)) ($k0 v4))))
