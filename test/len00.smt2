; Polymorphic parameters are currently replaced with ints

; (qualif ListZ ((v [@0]) (>= (len v) 0)))
(qualif ListZ ((v [int])) (>= (len v) 0))
; (qualif ListZ ((v [a])) (>= (len v) 0))

; uninterpreted function len that takes 2 polymorphic parameters,
; @(0) and @(1) and returns an int

; number is the # of polymorphic params
; TODO why is it explicitly stated?
; constant len : (func(2, [(@(0)  @(1)); int]))

; should the function type be separated?
; (uninterp len (@0 @1) int)
(uninterp len (int int) int)

; TODO add support for type constructors
; (wf [$k0] ((v [(Tuple int a)]))))
(wf [$k0] ((v [Silly int])))

(constraint
; (forall ((v [(Tuple int a)]))
  (forall ((v [(Silly int)]))
          (=> (>= (len v) 0) ([$k0]))))

(constraint
; (forall ((v [(Tuple int a)]))
  (forall ((v [(Silly int)]))
          (=> ([$k0]) (>= (len v) 0))))
