; Distinguish between binary ops
(qualif Pos   ((v int)) (<= 0 v))
(qualif Neg   ((v int)) (<= v 0))

; This should throw an error, invalid binary op
; (qualif Invalid ((v int))   (? v v))
