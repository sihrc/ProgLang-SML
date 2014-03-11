structure Primitives = struct

  structure I = InternalRepresentation
  structure S = StackRepresentation


  exception Primitive of string
  fun primError s = raise Primitive (s)

  (* 
   *   Primitive operations
   *)

  val trueVal = I.VInt 1
  val falseVal = I.VInt 0

  fun primAdd ((I.VInt i)::(I.VInt j)::stack) = (I.VInt (i+j))::stack
    | primAdd _ = primError "primAdd"

  fun primMul ((I.VInt i)::(I.VInt j)::stack) = (I.VInt (i*j))::stack
    | primMul _ = primError "primMul"

  fun primSub ((I.VInt i)::(I.VInt j)::stack) = (I.VInt (i-j))::stack
    | primSub _ = primError "primMul"

  fun primMod ((I.VInt i)::(I.VInt j)::stack) = (I.VInt (i mod j))::stack
    | primMod _ = primError "primMul"

  fun primDup (v::stack) = v::v::stack
    | primDup _ = primError "primDup"

  fun primSwap (v::w::stack) = w::v::stack
    | primSwap _ = primError "primSwap"

  fun primOver (v::w::stack) = w::v::w::stack
    | primOver _ = primError "primOver"

  fun primPick ((I.VInt n)::stack) = let
        fun loop 0 (v::_) = v
	  | loop n (v::st) = loop (n-1) st
	  | loop _ _ = primError "primPick - empty stack"
      in
        if n<0 then
	  primError "primPick - negative argument"
	else (loop n stack)::stack
      end
    | primPick _ = primError "primOver"

  fun primRot (u::v::w::stack) = w::u::v::stack
    | primRot _ = primError "primRot"

  fun primDrop (v::stack) = stack
    | primDrop _ = primError "primDrop"

  fun primZeroEq ((I.VInt 0)::stack) = trueVal::stack
    | primZeroEq (_::stack) = falseVal::stack
    | primZeroEq _ = primError "primZeroEq"

  fun primEq ((I.VInt i)::(I.VInt j)::stack) = 
        (if i=j then trueVal else falseVal)::stack
    | primEq ((I.VList l1)::(I.VList l2)::stack) = 
        (if l1=l2 then trueVal else falseVal)::stack
    | primEq _ = primError "primEq"

  fun primZeroGt ((I.VInt i)::stack) = if (i > 0) then trueVal::stack else falseVal::stack
    | primZeroGt (_::stack) = falseVal::stack
    | primZeroGt _ = primError "primZeroEq"

  fun primCons (v::(I.VList vs)::stack) = (I.VList (v::vs))::stack
    | primCons _ = primError "primCons"

  fun primHead ((I.VList (v::vs))::stack) = v::stack
    | primHead _ = primError "primHead"

  fun primTail ((I.VList (v::vs))::stack) = (I.VList vs)::stack
    | primTail _ = primError "primTail"

  fun primNilEq ((I.VList [])::stack) = trueVal::stack
    | primNilEq (_::stack) = falseVal::stack
    | primNilEq _ = primError "primNilEq"

  fun primShowStack stack = (print (" *** "^(S.stringOfStack stack 10)^"\n");
                             stack)

  fun primNil stack = (I.VList [])::stack

  fun primEmptyStack [] = [trueVal]
    | primEmptyStack stack = falseVal::stack



end
