(declare-sort Tuple 2)

(declare-fun len ((Set @1)) Int)

(qualif ListZ ((v (Set @0))) (>= (len v) 0))

(wf $k0 ((v (Set (Tuple Int @a)))))

(constraint
  (forall ((v (Set (Tuple Int @a))))
    (=> (>= (len v) 0)
        ($k0 v))))

(constraint
  (forall ((v (Set (Tuple Int @a))))
    (=> ($k0 v)
        (>= (len v) 0))))
