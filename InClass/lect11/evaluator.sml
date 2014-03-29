
structure Evaluator = struct

  structure I = InternalRepresentation



  exception Evaluation of string

  fun evalError msg = raise Evaluation msg


			 
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,sent)::env) = 
        if (n = name) then 
	  sent
	else lookup name env 




  (*
   *   Evaluation function
   * 
   *)


  fun execute env I.SEmpty stack = stack
    | execute env (I.SSequence (I.WInt i,ws)) stack = 
        execute env ws ((I.VInt i)::stack)
    | execute env (I.SSequence (I.WPrim prim, ws)) stack = 
        execute env ws (prim stack)
    | execute env (I.SSequence (I.WDefined w, ws)) stack = let
	val stack = execute env (lookup w env) stack
      in
	execute env ws stack
      end
    | execute env (I.SIf (trueS,falseS,thenS)) (v::stack) = let
	val stack = execute env (case v 
				  of I.VInt 0 => falseS
				   | _ => trueS) stack
      in
        execute env thenS stack
      end
    | execute _ _ _ = evalError "cannot execute"



  (* 
   *   Primitive operations
   *)

  val trueVal = I.VInt 1
  val falseVal = I.VInt 0

  fun primAdd ((I.VInt i)::(I.VInt j)::stack) = (I.VInt (i+j))::stack
    | primAdd _ = evalError "primAdd"

  fun primMul ((I.VInt i)::(I.VInt j)::stack) = (I.VInt (i*j))::stack
    | primMul _ = evalError "primMul"

  fun primSub ((I.VInt i)::(I.VInt j)::stack) = (I.VInt (i-j))::stack
    | primSub _ = evalError "primMul"

  fun primMod ((I.VInt i)::(I.VInt j)::stack) = (I.VInt (i mod j))::stack
    | primMod _ = evalError "primMul"

  fun primDup (v::stack) = v::v::stack
    | primDup _ = evalError "primDup"

  fun primSwap (v::w::stack) = w::v::stack
    | primSwap _ = evalError "primSwap"

  fun primOver (v::w::stack) = w::v::w::stack
    | primOver _ = evalError "primOver"

  fun primRot (u::v::w::stack) = w::u::v::stack
    | primRot _ = evalError "primRot"

  fun primDrop (v::stack) = stack
    | primDrop _ = evalError "primDrop"

  fun primZeroEq ((I.VInt 0)::stack) = trueVal::stack
    | primZeroEq (_::stack) = falseVal::stack
    | primZeroEq _ = evalError "primZeroEq"

  fun primZeroGt ((I.VInt i)::stack) = if (i > 0) then trueVal::stack else falseVal::stack
    | primZeroGt (_::stack) = falseVal::stack
    | primZeroGt _ = evalError "primZeroEq"

  fun primCons (v::(I.VList vs)::stack) = (I.VList (v::vs))::stack
    | primCons _ = evalError "primCons"

  fun primHead ((I.VList (v::vs))::stack) = v::stack
    | primHead _ = evalError "primHead"

  fun primTail ((I.VList (v::vs))::stack) = (I.VList vs)::stack
    | primTail _ = evalError "primTail"

  fun primNilEq ((I.VList [])::stack) = trueVal::stack
    | primNilEq (_::stack) = falseVal::stack
    | primNilEq _ = evalError "primNilEq"

  fun primNil stack = (I.VList [])::stack

  (* 
   *   Initial environment 
   *)

  val initialEnv = let
    fun p prim = I.SSequence (I.WPrim prim,I.SEmpty)
  in
      [ ("+", p primAdd),
        ("*", p primMul),
 	("-", p primSub),
	("mod", p primMod),
        ("dup", p primDup),
	("swap", p primSwap),
	("over", p primOver),
	("rot", p primRot),
	("0=", p primZeroEq),
	("0>", p primZeroGt),
	("drop", p primDrop),
	("cons", p primCons),
	("head", p primHead),
	("tail", p primTail),
	("nil=", p primNilEq),
	("nil", p primNil)
      ]
  end
				 
end
