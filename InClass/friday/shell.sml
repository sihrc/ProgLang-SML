

structure Shell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure E = Evaluator
  structure T = Transformations

  fun run fenv = let
    fun prompt () = (print "scheme-demo> "; TextIO.inputLine (TextIO.stdIn))
    fun pr l = print ((String.concatWith " " l)^"\n")
    fun read fenv = 
	(case prompt () 
	  of NONE => ()
	   | SOME ".\n" => ()
	   | SOME str => eval_print fenv str)
    and eval_print fenv str = 
	(let val ts = P.lexString str
	   (*  val _ = pr (["  Tokens ="] @ (map P.stringOfToken ts)) *)
	     val expr = P.parse ts
	     val _ = pr ["  IR = ", I.stringOfExpr (expr)]
	     val v = E.eval fenv expr
	     val _ = pr [I.stringOfValue v]
	 in
	   read fenv
	 end
	 handle P.Parsing msg => (pr ["Parsing error:", msg]; read fenv)
	      | E.Evaluation msg => (pr ["Evaluation error:", msg]; read fenv))
  in
    print "Type . by itself to quit\n";
    read fenv
  end
		   
end
