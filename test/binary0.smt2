; Distinguish between binary ops
(qualif Pos   ((v Int)) (<= 0 v))
(qualif Neg   ((v Int)) (<= v 0))

; This should throw an error, invalid binary op
; (qualif Invalid ((v Int))   (? v v))
