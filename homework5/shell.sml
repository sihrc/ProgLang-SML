(* 
 *   CODE FOR HOMEWORK 5
 *)



structure Shell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure E = Evaluator


  fun prompt () = (print "homework5> "; TextIO.inputLine (TextIO.stdIn))


  fun repl env = let
    fun pr l = print ((String.concatWith " " l)^"\n")
    fun process (P.DeclFunc (n,ps,body)) = let
          val env'  = E.addFunction env n ps body
	  val _ = pr ["Function",n,"added to environment"]
        in
          repl env'
        end
      | process (P.DeclProc (n,ps,body)) = let
          val env'  = E.addProcedure env n ps body
	  val _ = pr ["Procedure",n,"added to environment"]
        in
          repl env'
        end
      | process (P.DeclVar (n,e)) = let
	  val env' = E.addVariable env n e
	  val _ = pr ["Variable",n,"added to environment"]
	in
	  repl env'
	end
      | process (P.DeclConst (n,e)) = let
	  val env' = E.addConstant env n e
	  val _ = pr ["Constant",n,"added to environment"]
	in
	  repl env'
	end
      | process (P.DeclStmt stmt) = let 
	  val _ = E.exec env stmt
	in
	  repl env
	end
    fun repl' NONE = ()
      | repl' (SOME ".\n") = ()
      | repl' (SOME "?\n") = (E.info env; repl env)
      | repl' (SOME str) = 
	(case String.isPrefix ":parse " str
	  of true => let val stmt = P.parseStmt (P.lexString (String.extract (str, 6, NONE)))
			 val _ = pr [I.stringOfStmt stmt]
                     in
                       repl env
                     end
	   | _ => process (P.parseDecl (P.lexString str)))
	handle P.Parsing msg => (pr ["Parsing error:", msg]; repl env)
	     | E.Evaluation msg => (pr ["Evaluation error:", msg]; repl env)
  in
    repl' (prompt ())
  end


  fun run env = let
    val initEnv = E.initialEnv
  in
    print "Type . by itself to quit\n";
    print "Type ? by itself for environment information\n";
    print "Type :parse <stmt> to show parsing information\n";
    repl (env@initEnv)
  end
		   
end
