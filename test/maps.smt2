(constraint
  (forall ((v int)(m1 Map_t int int))
          (=> (&& (= v (Map_select m1 100))(= m1 (Map_default 0)))
              (= v 0))))

(constraint
  (forall ((v int)(m1 Map_t int int)(m2 Map_t int int))
          (=> (&& (= v (Map_select m2 100)) (&& (= m1 (Map_default 0)) (= m2 (Map_store (Map_store m1 10 1) 20 1))))
              (= v 0))))

(constraint
  (forall ((v int)(m1 Map_t int int)(m2 Map_t int int))
          (=> (&& (= v (Map_select m2 10)) (&& (= m1 (Map_default 0)) (= m2 (Map_store (Map_store m1 10 1) 20 1))))
              (= v 1))))

; TODO what if not all of the variables are used? should we perform some sort of culling when instantiating concrete sorts?
; v int is not used
(constraint
  (forall ((v int)(m1 Map_t int int)(m2 Map_t int int)(m3 Map_t int int))
          (=> (&& (= m1 (Map_default 0)) (&& (= m2 (Map_store (Map_store m1 10 1) 20 1)) (= m3 (Map_store (Map_store m1 20 1) 10 1))))
              (= m2 m3))))

(constraint
  (forall ((v int)(m1 Map_t int int)(m2 Map_t int int)(m3 Map_t int int)(m4 Map_t int int)(m5 Map_t int int))
          (=> (&& (= m1 (Map_default 0)) (&& (= m2 (Map_store (Map_store m1 10 1) 20 1))
                  (&& (= m3 (Map_store (Map_store m1 20 1) 10 1)) (&& (= m4 (Map_store m1 10 1)) (= m5 (Map_store m1 20 1))))))
              (= m2 (Map_union m4 m5)))))
