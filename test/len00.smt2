; Polymorphic parameters are currently replaced with ints

; (qualif ListZ ((v [@0]) (>= (len v) 0)))
(qualif ListZ ((v [Int])) (>= (len v) 0))
; (qualif ListZ ((v [a])) (>= (len v) 0))

; uninterpreted function len that takes 2 polymorphic parameters,
; @(0) and @(1) and returns an Int

; number is the # of polymorphic params
; TODO why is it explicitly stated?
; constant len : (func(2, [(@(0)  @(1)); Int]))

; should the function type be separated?
; (uninterp len (@0 @1) Int)
(uninterp len (Int Int) Int)

; TODO add support for type constructors
; (wf [$k0] ((v [(Tuple Int a)]))))
(wf [$k0] ((v [Silly Int])))

(constraint
(forall ((v [(Tuple Int a)]))
;  (forall ((v [(Silly Int)]))
          (=> (>= (len v) 0) ([$k0]))))

(constraint
; (forall ((v [(Tuple Int a)]))
  (forall ((v [(Silly Int)]))
          (=> ([$k0]) (>= (len v) 0))))
