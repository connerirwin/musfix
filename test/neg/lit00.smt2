; Uninterpreted Sorts
(declare-sort List 1)
(declare-sort Peano 0) ; why wasn't this emitted

; Constants
(declare-const one Peano)
(declare-const zero Peano)


; Distinct Constants
(assert (distinct one zero))


; Uninterpreted Functions


; Qualifiers


; Well-formedness constraints


; Horn constraints
(constraint
  (forall ((xZero Peano) (xOne Peano) (v1 Int))
    (=> (and (= xZero zero) (= xOne one))
        (= xZero xOne))))
