(qualif ListZ ((v [@0])) (>= (len v) 0))

(uninterp len (@0 @1) Int)

(wf [$k0] ((v [(Tuple Int a)])))

(constraint
(forall ((v [(Tuple Int a)]))
          (=> (>= (len v) 0) ([$k0]))))

(constraint
(forall ((v [(Tuple Int a)]))
          (=> ([$k0]) (>= (len v) 0))))
