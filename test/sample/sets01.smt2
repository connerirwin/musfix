; Convert Set to Set, parse Set Sort and Set Literal properly
; List are some constructed data type
(constraint
  (forall ((v Int) (m1 (Set Int)))
    (=> (= m1 {})
        (not (in 100 m1)))))
; In the negative version, the not has been removed 

(constraint
  (forall ((v Int) (m1 (Set Int)) (m2 (Set Int)))
    (=> (&& (= m1 {})
          (= m2 (union (union m1 (Set 10)) (Set 20))))
        (not (in 100 m2)))))

(constraint
  (forall ((v Int) (m1 (Set Int)) (m2 (Set Int)))
    (=> (&& (= m1 {})
          (= m2 (union (union m1 (Set 10)) (Set 20))))
        (in 10 m2))))

(constraint
  (forall ((v Int) (m1 (Set Int)) (m2 (Set Int)) (m3 (Set Int)))
    (=> (&& (&& (= m1 {})
          (= m2 (union (union m1 (Set 10)) (Set 20))))
          (= m3 (union (union m1 (Set 20)) (Set 10))))
        (= m2 m3))))

(constraint
  (forall ((v Int) (m1 (Set Int)) (m2 (Set Int)) (m3 (Set Int))
            (m4 (Set Int)) (m5 (Set Int)))
    (=> (&& (&& (&& (&& (= m1 {})
          (= m2 (union (union m1 (Set 10)) (Set 20))))
          (= m3 (union (union m1 (Set 20)) (Set 10))))
          (= m4 (union m1 (Set 10))))
          (= m5 (union m1 (Set 20))))
        (= m2 (union m4 m5)))))

; (constraint
;   (forall ()
;     (=> True False)))
