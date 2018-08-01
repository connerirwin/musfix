; Distinguish between unary ops
(qualif Neg     ((b Bool))  (- b))
(qualif Not     ((b Bool))  (not b))

; This should throw an error, invalid unary op
; (qualif Invalid ((v Int))   (+ v))
