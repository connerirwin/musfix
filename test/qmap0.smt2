; Not all qualifiers need to be used by each wf constraint
(qualif Discard ((b bool))        (= b b))
(qualif Partial ((v int)(b bool)) (not (= v b)))
(qualif Accept  ((v int))         (<= v 0))

(wf $k0 ((v0 int)))
(wf $k1 ((b0 bool)))
(wf $k2 ((v1 int)(b1 bool)))