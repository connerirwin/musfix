(constraint
  (forall ((v Int) (m1 (Set Int)))
    (=> (= m1 (Set_empty 0))
        (not (in 100 m1)))))

(constraint
  (forall ((v Int) (m1 (Set Int)) (m2 (Set Int)))
    (=> (&& (= m1 (Set_empty 0))
          (= m2 (union (union m1 (Set_sng 10)) (Set_sng 20))))
        (not (in 100 m2)))))

(constraint
  (forall ((v Int) (m1 (Set Int)) (m2 (Set Int)))
    (=> (&& (= m1 (Set_empty 0))
          (= m2 (union (union m1 (Set_sng 10)) (Set_sng 20))))
        (in 10 m2))))

(constraint
  (forall ((v Int) (m1 (Set Int)) (m2 (Set Int)) (m3 (Set Int)))
    (=> (&& (&& (= m1 (Set_empty 0))
          (= m2 (union (union m1 (Set_sng 10)) (Set_sng 20))))
          (= m3 (union (union m1 (Set_sng 20)) (Set_sng 10))))
        (= m2 m3))))

(constraint
  (forall ((v Int) (m1 (Set Int)) (m2 (Set Int)) (m3 (Set Int))
            (m4 (Set Int)) (m5 (Set Int)))
    (=> (&& (&& (&& (&& (= m1 (Set_empty 0))
          (= m2 (union (union m1 (Set_sng 10)) (Set_sng 20))))
          (= m3 (union (union m1 (Set_sng 20)) (Set_sng 10))))
          (= m4 (union m1 (Set_sng 10))))
          (= m5 (union m1 (Set_sng 20))))
        (= m2 m3))))
