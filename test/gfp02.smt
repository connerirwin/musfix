(qualif Pos   ((v int)) (<= 0 v))
(qualif Neg   ((v int)) (<= v 0))
(qualif NeqZ  ((v int)) (not (= v 0)))
(qualif GtTen ((v int)) (< v 10))
(qualif False ()        (= 66 77))

(wf $k0 ((v0 int)))

(constraint
  (forall (v1 int)
          (=> ($k0 v1) (< 0 (+ v1 1)))))  ; 0 < (v1 + 1)
          
(constraint
  (forall (v3 int)
          (=> ($k0 v3) (> 11 (+ v3 1))))) ; 11 > (v3 + 1)

(constraint
  (forall (v2 int)
          (=> (= v2 9) ($k0 v2))))
          
(constraint
  (forall (v4 int)
        (=> (&& (< v4 9) (> v4 3)) ($k0 v4))))
