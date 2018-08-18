; What if we allow uninterpreted functions that take no args, and are just constants?
; (declare-const m1 (Map_t Int Int) (Map_default 0))
; (declare-const name sort value)

(constraint
  (forall ((v Int) (m1 (Map_t Int Int)))
    (=> (&& (= v (Map_select m1 100)) (= m1 (Map_default 0)))
        (= v 0))))

(constraint
  (forall ((v Int) (m1 (Map_t Int Int)) (m2 (Map_t Int Int)))
    (=> (&& (&& (= v (Map_select m2 100)) (= m1 (Map_default 0)))
          (= m2 (Map_store (Map_store m1 10 1) 20 1)))
        (= v 0))))

(constraint
  (forall ((v Int) (m1 (Map_t Int Int)) (m2 (Map_t Int Int)))
    (=> (&& (&& (= v (Map_select m2 10)) (= m1 (Map_default 0)))
          (= m2 (Map_store (Map_store m1 10 1) 20 1)))
        (= v 1))))

(constraint
  (forall ((v Int) (m1 (Map_t Int Int)) (m2 (Map_t Int Int)) (m3 (Map_t Int Int)))
    (=> (&& (&& (= m1 (Map_default 0)) (= m2 (Map_store (Map_store m1 10 1) 20 1)))
          (= m3 (Map_store (Map_store m1 20 1) 10 1)))
        (= m2 m3))))

; TODO implement map_union
; (constraint
;   (forall ((v Int) (m1 (Map_t Int Int)) (m2 (Map_t Int Int)) (m3 (Map_t Int Int)) (m4 (Map_t Int Int)) (m5 (Map_t Int Int)))
;     (=> (&& (= m1 (Map_default 0)) (&& (= m2 (Map_store (Map_store m1 10 1) 20 1))
;           (&& (= m3 (Map_store (Map_store m1 20 1) 10 1))
;           (&& (= m4 (Map_store m1 10 1)) (= m5 (Map_store m1 20 1))))))
;         (= m2 (Map_union m4 m5)))))
