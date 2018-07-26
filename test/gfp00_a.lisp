(qualif Pos   ((v int)) (<= 0 v))
(qualif Neg   ((v int)) (<= v 0))
(qualif NeqZ  ((v int)) (not (= v 0)))
(qualif False ()        (= 66 77))

(wf $k0 ((v1 int)))

(constraint
  (forall (v1 int)
          (=> ($k0 v1) (< 0 (+ v1 1)))))

;(constraint
;  (forall (v1 int)
;          (=> (= v1 10) ($k0 v1))))
