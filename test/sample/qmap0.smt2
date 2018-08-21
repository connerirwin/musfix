; Not all qualifiers need to be used by each wf constraint
(qualif Discard ((b Bool))        (= b b))
; (qualif Partial ((v Int)(b Bool)) (not (= v b)))
(qualif Accept  ((v Int))         (<= v 0))

(wf $k0 ((v0 Int)))
(wf $k1 ((b0 Bool)))
(wf $k2 ((v1 Int)(b1 Bool)))
