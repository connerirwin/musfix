(qualif Discard ((b bool))        (= b b)) ; this causes unifySorts to fail
;(qualif Partial ((v int)(b bool)) (not (= v b)))
(qualif Accept  ((v int))         (<= v 0))

(wf $k0 ((v0 int)))
