(* 
 *   CODE FOR HOMEWORK 4
 *)



structure Shell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure E = Evaluator

  (* automatically loads primitives defined in the evaluator *)

  fun run env = let
    fun prompt () = (print "homework4> "; TextIO.inputLine (TextIO.stdIn))
    fun pr l = print ((String.concatWith " " l)^"\n")
    fun read fenv = 
	(case prompt () 
	  of NONE => ()
	   | SOME ".\n" => ()
	   | SOME str => eval_print fenv str)
    and eval_print fenv str = 
        (case String.isPrefix ":parse " str
	  of true => let val ts = P.lexString (String.extract (str, 6, NONE))
                         val _ = pr (["Tokens ="] @ (map P.stringOfToken ts)) 
	                 val expr = P.parse ts
	                 val _ = pr [I.stringOfExpr (expr)]
                     in
                        read fenv
                     end
           | false => let val ts = P.lexString str
                          val expr = P.parse ts
	                  val v = E.eval fenv expr
 	                  val _ = pr [I.stringOfValue v]
	              in
	                read fenv
	              end)
	  handle P.Parsing msg => (pr ["Parsing error:", msg]; read fenv)
	       | E.Evaluation msg => (pr ["Evaluation error:", msg]; read fenv)
    val initEnv = E.initialEnv@env
  in
    print "Type . by itself to quit\n";
    print "Type :parse <expr> to see the parse of expression <expr>\n";
    print "Initial environment: "; 
    app (fn (s,_) => (print (s^" "))) initEnv;
    print "\n";
    read initEnv
  end
		   
end
