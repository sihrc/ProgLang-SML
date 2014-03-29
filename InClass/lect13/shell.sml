

structure Shell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure S = StackRepresentation
  structure E = Evaluator
  structure C = Compiler
  structure Pr = Primitives



  fun prompt () = (print "expr";
		   print E.shellSuffix;
		   print "> ";
		   TextIO.inputLine (TextIO.stdIn))


  fun repl env = let
    fun pr l = print ((String.concatWith " " l)^"\n")
    fun process (P.DDef (s,params,e)) = let
          val env'  = E.addDefinition env s params e
	  val _ = pr ["Definition",s,"added to environment"]
        in
          repl env'
        end
      | process (P.DExpr e) =  let
	  val v = E.execute env e
	  val _ = pr [I.stringOfValue v]
	in
	  repl env
	end
    fun repl' NONE = ()
      | repl' (SOME ".\n") = ()
      | repl' (SOME "?\n") = (E.info env; repl env)
      | repl' (SOME str) = 
	  (process (P.parseDecl str)
	   handle P.Parsing msg => (pr ["Parsing error:", msg]; repl env)
		| E.Evaluation msg => (pr ["Evaluation error:", msg]; repl env)
		| Pr.Primitive msg => (pr ["Evaluation error:", msg]; repl env)
		| C.Compilation msg => (pr ["Compilation error:", msg]; repl env)
		| IO.Io _ => (pr ["I/O error"]; repl env))
  in 
    repl' (prompt ())
  end



  fun run env = let
    val initEnv = E.initialEnv
  in
    print "Type . by itself to quit\n";
    print "Type ? by itself for environment information\n";
    print "Type :parse <expr> to see the parse of expression <expr>\n";
    repl initEnv
  end
	   
end
