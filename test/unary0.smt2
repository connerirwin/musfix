; Distinguish between unary ops
(qualif Neg     ((b bool))  (- b))
(qualif Not     ((b bool))  (not b))

; This should throw an error, invalid unary op
; (qualif Invalid ((v int))   (+ v))
