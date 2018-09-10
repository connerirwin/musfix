(qualif Equiv ((v @a)(z @b)) (= v z))
(qualif False ((a @a)(b @b)) False)

(wf $k0 ((v Int)(x Int)))

(constraint
  (forall ((v Int) (q Int) (m1 (Map Int Int)))
    (=> (&& (&& (= v (Map_select m1 100)) (= m1 (Map_default 0))) (= q 0))
        ($k0 v q))))

(constraint
  (forall ((m1 (Map Bool Bool)))
    (=> (= m1 (Map_default True))
        (= (Map_select m1 False) True))))
