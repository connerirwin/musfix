; Should these literals be built in?
(declare-fun Set_empty (Int) Set_Set @a)
(declare-fun Set_sng   (@a)  Set_Set @a)

(constraint
  (forall ((v Int) (m1 (Set_Set Int)))
    (=> (= m1 (Set_empty 0))
        (not (Set_mem 100 m1)))))

(constraint
  (forall ((v int) (m1 (Set_Set Int)) (m2 (Set_Set Int)))
    (=> (&& (= m1 (Set_empty 0)) (= m2 (Set_cup (Set_cup m1 (Set_sng 10)) (Set_sng 20))))
        (not (Set_mem 100 m2)))))

(constraint
  (forall ((v int) (m1 (Set_Set Int) (m2 (Set_Set Int))))
    (=> (&& (= m1 (Set_empty 0)) (= m2 (Set_cup (Set_cup m1 (Set_sng 10)) (Set_sng 20))))
        (Set_mem 10 m2))))

(constraint
  (forall ((v int) (m1 (Set_Set Int)) (m2 (Set_Set Int)) (m3 (Set_Set Int)))
    (=> (&& (= m1 (Set_empty 0)) (&& (= m2 (Set_cup (Set_cup m1 (Set_sng 10)) (Set_sng 20)))
          (= m3 (Set_cup (Set_cup m1 (Set_sng 20)) (Set_sng 10)))))
        (= m2 m3))))

(constraint
  (forall ((v int) (m1 (Set_Set Int)) (m2 (Set_Set Int)) (m3 (Set_Set Int))
            (m4 (Set_Set Int)) (m5 (Set_Set Int)))
    (=> (&& (= m1 (Set_empty 0)) (&& (= m2 (Set_cup (Set_cup m1 (Set_sng 10)) (Set_sng 20)))
          (&& (= m3 (Set_cup (Set_cup m1 (Set_sng 20)) (Set_sng 10)))
          (&& (= m4 (Set_cup m1 (Set_sng 10)) (= m5 (Set_cup m1 (Set_sng 20))))))))
        (= m2 m3))))
