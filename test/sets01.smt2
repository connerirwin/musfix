; Convert Set_t to Set, parse Set Sort and Set Literal properly
(constraint
  (forall ((v Int) (m1 (Set_t Int)))
    (=> (= m1 [])
        (not (in 100 m1)))))

(constraint
  (forall ((v Int) (m1 (Set_t Int)) (m2 (Set_t Int)))
    (=> (&& (= m1 [])
          (= m2 (union (union m1 (Set 10)) (Set 20))))
        (not (in 100 m2)))))

(constraint
  (forall ((v Int) (m1 (Set_t Int)) (m2 (Set_t Int)))
    (=> (&& (= m1 [])
          (= m2 (union (union m1 (Set 10)) (Set 20))))
        (in 10 m2))))

(constraint
  (forall ((v Int) (m1 (Set_t Int)) (m2 (Set_t Int)) (m3 (Set_t Int)))
    (=> (&& (&& (= m1 [])
          (= m2 (union (union m1 (Set 10)) (Set 20))))
          (= m3 (union (union m1 (Set 20)) (Set 10))))
        (= m2 m3))))

(constraint
  (forall ((v Int) (m1 (Set_t Int)) (m2 (Set_t Int)) (m3 (Set_t Int))
            (m4 (Set_t Int)) (m5 (Set_t Int)))
    (=> (&& (&& (&& (&& (= m1 [])
          (= m2 (union (union m1 (Set 10)) (Set 20))))
          (= m3 (union (union m1 (Set 20)) (Set 10))))
          (= m4 (union m1 (Set 10))))
          (= m5 (union m1 (Set 20))))
        (= m2 m3))))
