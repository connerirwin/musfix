; Distinguish between unary ops
(qualif Neg ((i Int))  (- i))
(qualif Not ((b Bool)) (not b))

; Various errors
; (qualif _Neg ((b Bool)) (- b))
; (qualif _Not ((i Int)) (not i))
