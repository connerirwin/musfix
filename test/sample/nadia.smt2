; the weakest solution is `$k0 -> len v0 == 1 + len nil, elems v0 == v1 + elems nil`
; x:x:[]


; num is the number of sorts that this takes
(declare-sort List 1)

(declare-fun len   ((List @0)) Int)
(declare-fun elems ((List @0)) (Set @0))

(qualif LenZ  ((x (List @0)))                      (= (len x) 0))
(qualif Empty ((x (List @0)))                      (= (elems x) []))
(qualif Plus1 ((x (List @0)) (y (List @0)))        (= (len x) (+ 1 (len y))))
(qualif SumL  ((x (List @0)) (y (List @0)) (z @0)) (= (elems x) (union (Set z) (elems y))))

(wf $k0 ((nil (List @0)) (v0 (List @0)) (v1 Int)))

(constraint
  (forall ((x Int) (nil (List @0)) (x4 (List @0)) (_v (List @0)))
    (=> (&& (&& (&& (&& (= (elems nil) []) (= (len nil) 0)) ($k0 nil x4 x))
          (= (len _v) (+ 1 (len x4)))) (>= (len x4) 0))
        (= (len _v) 2))))

(constraint
  (forall ((x Int) (nil (List @0)) (x4 (List @0)) (_v (List @0)))
    (=> (&& (&& (&& (&& (= (elems nil) []) (= (len nil) 0)) ($k0 nil x4 x))
          (= (elems _v) (union (Set x) (elems x4)))) (>= (len x4) 0))
        (= (elems _v) (Set x)))))
