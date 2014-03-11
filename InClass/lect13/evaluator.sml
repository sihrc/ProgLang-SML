
structure Evaluator = struct

  structure I = InternalRepresentation
  structure S = StackRepresentation
  structure C = Compiler
  structure P = Primitives


  exception Evaluation of string

  fun evalError msg = raise Evaluation msg



			 
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,sent)::env) = 
        if (n = name) then 
	  sent
	else lookup name env 




  fun execute' env S.SEmpty stack = stack
    | execute' env (S.SSequence (S.WInt i,ws)) stack = 
        execute' env ws ((I.VInt i)::stack)
    | execute' env (S.SSequence (S.WPrim (_,prim), ws)) stack = 
        execute' env ws (prim stack)
    | execute' env (S.SSequence (S.WDefined w, ws)) stack = let
	val stack = execute' env (lookup w env) stack
      in
        execute' env ws stack
      end
    | execute' env (S.SIf (trueS,falseS,thenS)) (v::stack) = let
        val stack = execute' env (case v 
			      of I.VInt 0 => falseS
			       | _ => trueS) stack
      in
	execute' env thenS stack
      end
    | execute' env (S.SWhile (whileS,thenS)) stack  = let
	  fun loop ((I.VInt 0)::stack) = stack
	    | loop (_::stack) = loop (execute' env whileS stack)
	    | loop _ = evalError "stack empty on while"
      in
	  execute' env thenS (loop stack)
      end
    | execute' _ _ _ = evalError "cannot execute"



  fun execute env expr = let
    val sent = C.compileExpr expr
    val stack = execute' env sent []
  in
    case stack
     of [] => evalError "Expression returned no value!"
      | (v::[]) => v
      | (v::_) => (print "Warning - stack not empty\n"; v)
  end




  (* 
   *   Initial environment 
   *)

  val initialEnv = let
    fun process (n,prim) = (n,S.SSequence (S.WPrim (n,prim),S.SEmpty))
    val primitives = [ ("+", P.primAdd),
		       ("*", P.primMul),
 		       ("-", P.primSub),
		       ("mod", P.primMod),
		       ("dup", P.primDup),
		       ("swap", P.primSwap),
		       ("over", P.primOver),
		       ("rot", P.primRot),
		       ("pick", P.primPick),
		       ("=", P.primEq),
		       ("0=", P.primZeroEq),
		       ("0>", P.primZeroGt),
		       ("drop", P.primDrop),
		       ("cons", P.primCons),
		       ("head", P.primHead),
		       ("tail", P.primTail),
		       ("nil=", P.primNilEq),
		       ("nil", P.primNil),
		       ("empty-stack", P.primEmptyStack),
		       ("show-stack", P.primShowStack)
		     ] 
  in
      map process primitives
  end

  val shellSuffix = ""

  fun addDefinition env name params body = let
    val sent = C.compileDef name params body
  in
    (name,sent)::env
  end


  fun info env = (app (fn (s,_) => (print (" "^s^"\n"))) env)


				 
end
