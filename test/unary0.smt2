; Distinguish between unary ops
(qualif Neg ((i Int))  (- i))
(qualif Not ((b Bool)) (not b))

; Various Errors errors
; (qualif _Neg ((b Bool)) (- b))
; (qualif _Not ((i Int)) (not i))

; This should throw an error, invalid unary op
; (qualif Invalid ((v Int))   (+ v))
