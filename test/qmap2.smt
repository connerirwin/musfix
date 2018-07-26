; Formal parameter names can collide in qualifiers
(qualif Less ((a int) (b int))   (< a b))
(qualif Or   ((a bool)(b bool)) (|| a b))

; And also in the wf constaints
(wf $k0 ((v0 int) (v1 int)))
(wf $k1 ((v0 bool)(v1 bool)))
