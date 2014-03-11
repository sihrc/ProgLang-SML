

structure Shell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure E = Evaluator



  fun run env = let
    fun prompt () = (print "stack> "; TextIO.inputLine (TextIO.stdIn))
    fun pr l = print ((String.concatWith " " l)^"\n")
    fun read (env,stack) = 
	(case prompt () 
	  of NONE => ()
	   | SOME ".\n" => ()
	   | SOME str => eval_print (env,stack) str)
    and eval_print (env,stack) str = 
	(case P.parseDecl (P.lexString str)
	  of P.DDef (w,ws) => let
	       val _ = pr ["Definition",w,"added to environment"]
	     in
	       read ((w,ws)::env,stack)
	     end
	   | P.DSent ws => let
	       val stack = E.execute env ws stack
	       val _ = pr [I.stringOfStack stack 10]
	     in
	       read (env,stack)
	     end)
	handle P.Parsing msg => (pr ["Parsing error:", msg]; read (env,stack))
	     | E.Evaluation msg => (pr ["Evaluation error:", msg]; read (env,stack))
	     | IO.Io _ => (pr ["I/O error"]; read (env,stack))
    val initEnv = E.initialEnv @ env
  in
    print "Type . by itself to quit\n";
    print "Type :parse <expr> to see the parse of expression <expr>\n";
    print "Initial environment: "; 
    app (fn (s,_) => (print (s^" "))) initEnv;
    print "\n";
    read (initEnv, [])
  end
	   
end
