; Formal parameter names can collide in qualifiers
(qualif Less ((a Int) (b Int))   (< a b))
(qualif Or   ((a Bool)(b Bool)) (|| a b))

; And also in the wf constaints
(wf $k0 ((v0 Int) (v1 Int)))
(wf $k1 ((v0 Bool)(v1 Bool)))
