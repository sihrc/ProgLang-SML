
structure Parser =  struct

  (*
   *  Wrapper around the regexp library
   *)	   

  structure R = RegExpFn (structure P = AwkSyntax structure E = DfaEngine)

  structure I = InternalRepresentation
		
  (* match a compiled regular expression against a list of characters *)
		
  fun matchRE' re cs = let
    val prefix = R.prefix re List.getItem
    fun getMatch NONE = NONE
      | getMatch (SOME (mt, cs')) = let
	  val {pos,len} = MatchTree.root mt
	in
	  SOME (implode (List.take (pos,len)), cs')
	end
  in
    getMatch (prefix cs)
  end
		       
  (* match a string regular expression against a list of characters *)
		       
  fun matchRE re cs = matchRE' (R.compileString re) cs



  exception Parsing of string

  fun parseError msg = raise Parsing msg
			 
			 


  (* 
   *   A simple lexer
   *
   *)

  datatype token = T_LET 
		 | T_SYM of string 
		 | T_INT of int 
		 | T_TRUE 
		 | T_FALSE
		 | T_PLUS
		 | T_MINUS
		 | T_TIMES
		 | T_EQUAL
		 | T_IF 
		 | T_LPAREN 
		 | T_RPAREN
		 | T_CONS
		 | T_HEAD
		 | T_TAIL
		 | T_NIL
		 | T_DEFINE


  fun stringOfToken T_LET = "T_LET"
    | stringOfToken (T_SYM s) = "T_SYM["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken T_TRUE = "T_TRUE"
    | stringOfToken T_FALSE = "T_FALSE"
    | stringOfToken T_PLUS = "T_PLUS"
    | stringOfToken T_MINUS = "T_MINUS"
    | stringOfToken T_TIMES = "T_TIMES"
    | stringOfToken T_EQUAL = "T_EQUAL"
    | stringOfToken T_IF  = "T_IF"
    | stringOfToken T_LPAREN = "T_LPAREN"
    | stringOfToken T_RPAREN = "T_RPAREN"
    | stringOfToken T_CONS = "T_CONS"
    | stringOfToken T_HEAD = "T_HEAD"
    | stringOfToken T_TAIL = "T_TAIL"
    | stringOfToken T_NIL = "T_NIL"
    | stringOfToken T_DEFINE = "T_DEFINE"
			       

		   
  fun whitespace _ = NONE
		     
  fun produceSymbol "let" = SOME (T_LET)
    | produceSymbol "true" = SOME (T_TRUE)
    | produceSymbol "false" = SOME (T_FALSE)
    | produceSymbol "if" = SOME (T_IF)
    | produceSymbol "cons" = SOME (T_CONS)
    | produceSymbol "head" = SOME (T_HEAD)
    | produceSymbol "tail" = SOME (T_TAIL)
    | produceSymbol "nil" = SOME (T_NIL)
    | produceSymbol "define" = SOME (T_DEFINE)
    | produceSymbol text = SOME (T_SYM text)
			   
  fun produceInt text = (case Int.fromString text
			  of NONE => parseError "integer literal out of bounds"
			   | SOME i => SOME (T_INT i))
			
  fun producePlus _ = SOME (T_PLUS)
  fun produceMinus _ = SOME (T_MINUS)
  fun produceTimes _ = SOME (T_TIMES)
  fun produceEqual _ = SOME (T_EQUAL)
  fun produceLParen _ = SOME (T_LPAREN)
  fun produceRParen _ = SOME (T_RPAREN)
		       
		       
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",         whitespace),
		 ("\\+" ,                 producePlus),
		 ("-" ,                   produceMinus),
		 ("\\*",                  produceTimes),
		 ("=",                    produceEqual),
		 ("[a-zA-Z][a-zA-Z0-9]*", produceSymbol),
		 ("~?[0-9]+",             produceInt),
		 ("\\(",                  produceLParen),
		 ("\\)",                  produceRParen)]
  end
	       
	       
  fun getToken cs = let
    fun loop [] = parseError ("cannot tokenize "^(implode cs))
      | loop ((re,f)::xs) = (case matchRE' re cs
			      of NONE => loop xs
			       | SOME (m,cs') => (f m,cs'))
  in
    loop tokens
  end
		    
		    
  fun lex []  = []
    | lex cs = let
	val (token,cs') = getToken cs
      in
        case token 
	 of NONE => lex cs'
	  | SOME t => t::(lex cs')
      end
	       
	       
  fun lexString str = lex (explode str)
		      
		      
			   

  (* 
   *   A SIMPLE PARSER FOR A SCHEME-SUBSET
   * 
   *   Grammar:
   * 
   *   decl ::= T_LPAREN T_DEFINE T_LPAREN T_SYM sym_list T_RPAREN expr T_RPAREN
   *            expr
   * 
   *   expr ::= T_INT 
   *            T_TRUE
   *            T_FALSE
   *            T_SYM 
   *            T_NIL
   *            T_LPAREN T_LET T_LPAREN T_LPAREN T_SYM expr T_RPAREN T_RPAREN expr T_RPAREN
   *            T_LPAREN T_IF expr expr expr T_RPAREN
   *            T_LPAREN T_PLUS expr expr T_RPAREN
   *            T_LPAREN T_MINUS expr expr T_RPAREN
   *            T_LPAREN T_TIMES expr expr T_RPAREN
   *            T_LPAREN T_EQUAL expr expr T_RPAREN
   *            T_LPAREN T_CONS expr expr T_RPAREN
   *            T_LPAREN T_HEAD expr T_RPAREN
   *            T_LPAREN T_TAIL expr T_RPAREN
   *            T_LPAREN T_SYM expr_list T_RPAREN
   *
   *   expr_list ::= expr expr_list
   *                 expr
   *
   *   sym_list ::= T_SYM sym_list
   *                T_SYM
   *)
			   

  datatype decl = DDef of string * string list * I.expr
                | DExpr of I.expr

			   
  fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
    | expect_INT _ = NONE
		     
  fun expect_TRUE (T_TRUE::ts) = SOME ts
    | expect_TRUE _ = NONE
		      
  fun expect_FALSE (T_FALSE::ts) = SOME ts
    | expect_FALSE _ = NONE
		       
  fun expect_SYM ((T_SYM s)::ts) = SOME (s,ts)
    | expect_SYM _ = NONE
		     
  fun expect_IF (T_IF::ts) = SOME ts
    | expect_IF _ = NONE
		    
  fun expect_LET (T_LET::ts) = SOME ts
    | expect_LET _ = NONE
		     
  fun expect_EQUAL (T_EQUAL::ts) = SOME ts
    | expect_EQUAL _ = NONE
		       
  fun expect_LPAREN (T_LPAREN::ts) = SOME ts
    | expect_LPAREN _ = NONE
			
  fun expect_RPAREN (T_RPAREN::ts) = SOME ts
    | expect_RPAREN _ = NONE
			
  fun expect_PLUS (T_PLUS::ts) = SOME ts
    | expect_PLUS _ = NONE

  fun expect_MINUS (T_MINUS::ts) = SOME ts
    | expect_MINUS _ = NONE
		      
  fun expect_TIMES (T_TIMES::ts) = SOME ts
    | expect_TIMES _ = NONE

  fun expect_CONS (T_CONS::ts) = SOME ts
    | expect_CONS _ = NONE

  fun expect_HEAD (T_HEAD::ts) = SOME ts
    | expect_HEAD _ = NONE

  fun expect_TAIL (T_TAIL::ts) = SOME ts
    | expect_TAIL _ = NONE

  fun expect_NIL (T_NIL::ts) = SOME ts
    | expect_NIL _ = NONE

  fun expect_DEFINE (T_DEFINE::ts) = SOME ts
    | expect_DEFINE _ = NONE


  fun choose [] ts = NONE
    | choose (parser::parsers) ts = 
      (case parser ts
	of NONE => choose parsers ts
	 | s => s)

  fun parse_decl ts = let
    fun decl_def ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_DEFINE ts
	       of NONE => NONE
		| SOME ts => 
		  (case expect_LPAREN ts
		    of NONE => NONE
		     | SOME ts => 
		       (case expect_SYM ts
			of NONE => NONE
			 | SOME (n,ts) => 
			   (case parse_sym_list ts
			     of NONE => NONE
			      | SOME (params,ts) => 
				(case expect_RPAREN ts
				  of NONE => NONE
				   | SOME ts => 
				     (case parse_expr ts
				       of NONE => NONE
					| SOME (e,ts) => 
					  (case expect_RPAREN ts
					    of NONE => NONE
					     | SOME ts => 
					         SOME (DDef (n,params,e),ts)))))))))
    fun decl_expr ts = 
	(case parse_expr ts
	  of NONE => NONE
	  | SOME (e,ts) => SOME (DExpr e,ts))
  in
    choose [decl_def,decl_expr] ts
  end


  and parse_expr ts = let
    fun expr_int ts = 
	(case expect_INT ts
	  of NONE => NONE
	  | SOME (i,ts) => SOME (I.EVal (I.VInt i),ts))
    fun expr_true ts = 
	(case expect_TRUE ts
	  of NONE => NONE
	   | SOME ts => SOME (I.EVal (I.VBool true),ts))
    fun expr_false ts = 
	(case expect_FALSE ts
	  of NONE => NONE
	   | SOME ts => SOME (I.EVal (I.VBool false),ts))
    fun expr_sym ts = 
	(case expect_SYM ts
	  of NONE => NONE
	   | SOME (s,ts) => SOME (I.EIdent s,ts))
    fun expr_nil ts = 
	(case expect_NIL ts
	  of NONE => NONE
	   | SOME ts => SOME (I.EVal (I.VList []),ts))
    fun expr_let ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_LET ts
	       of NONE => NONE
		| SOME ts => 
		  (case expect_LPAREN ts
		    of NONE => NONE
		     | SOME ts =>
		       (case expect_LPAREN ts
			 of NONE => NONE
			  | SOME ts => 
			    (case expect_SYM ts
			      of NONE => NONE
			       | SOME (s,ts) => 
				 (case parse_expr ts
				   of NONE => NONE
				    | SOME (e,ts) => 
				      (case expect_RPAREN ts
					of NONE => NONE
					 | SOME ts => 
					   (case expect_RPAREN ts
					     of NONE => NONE
					      | SOME ts => 
						(case parse_expr ts
						  of NONE => NONE
						   | SOME (body,ts) =>
						     (case expect_RPAREN ts
						       of NONE => NONE
						       | SOME ts => 
							   SOME (I.ELet (s,e,body),ts)))))))))))
    fun expr_if ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_IF ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr ts
		    of NONE => NONE
		     | SOME (e1,ts) => 
		       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e2,ts) => 
			    (case parse_expr ts
			      of NONE => NONE
			       | SOME (e3,ts) => 
				 (case expect_RPAREN ts
				   of NONE => NONE
				    | SOME ts => SOME (I.EIf (e1,e2,e3),ts)))))))
    fun expr_plus ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_PLUS ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr ts
		    of NONE => NONE
		     | SOME (e1,ts) => 
		       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e2,ts) => 
			    (case expect_RPAREN ts
			      of NONE => NONE
			       | SOME ts => SOME (I.EAdd (e1,e2),ts))))))
    fun expr_minus ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_MINUS ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr ts
		    of NONE => NONE
		     | SOME (e1,ts) => 
		       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e2,ts) => 
			    (case expect_RPAREN ts
			      of NONE => NONE
			       | SOME ts => SOME (I.ESub (e1,e2),ts))))))
    fun expr_times ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_TIMES ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr ts
		    of NONE => NONE
		     | SOME (e1,ts) => 
		       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e2,ts) => 
			    (case expect_RPAREN ts
			      of NONE => NONE
			       | SOME ts => SOME (I.EMul (e1,e2),ts))))))
    fun expr_equal ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_EQUAL ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr ts
		    of NONE => NONE
		     | SOME (e1,ts) => 
		       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e2,ts) => 
			    (case expect_RPAREN ts
			      of NONE => NONE
			       | SOME ts => SOME (I.EEq (e1,e2),ts))))))
    fun expr_cons ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_CONS ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr ts
		    of NONE => NONE
		     | SOME (e1,ts) => 
		       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e2,ts) => 
			    (case expect_RPAREN ts
			      of NONE => NONE
			       | SOME ts => SOME (I.ECons (e1,e2),ts))))))
    fun expr_head ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_HEAD ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr ts
		    of NONE => NONE
		     | SOME (e1,ts) => 
		       (case expect_RPAREN ts
			 of NONE => NONE
			  | SOME ts => SOME (I.EHead (e1),ts)))))
    fun expr_tail ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_TAIL ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr ts
		    of NONE => NONE
		     | SOME (e1,ts) => 
		       (case expect_RPAREN ts
			 of NONE => NONE
			  | SOME ts => SOME (I.ETail (e1),ts)))))
    fun expr_call ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_SYM ts
	       of NONE => NONE
		| SOME (s,ts) => 
		  (case parse_expr_list ts
		    of NONE => NONE
		     | SOME (es,ts) => 
		       (case expect_RPAREN ts
			 of NONE => NONE
			  | SOME ts => SOME (I.ECall (s,es),ts)))))
  in
    choose [expr_int,expr_true,expr_false,expr_sym,expr_nil,
	    expr_let,expr_if,
	    expr_plus,expr_minus,expr_times,
	    expr_equal,expr_cons,expr_head,expr_tail,expr_call] ts
  end


  and parse_expr_list ts = 
      (case parse_expr ts
	of NONE => NONE
	 | SOME (e,ts) => 
	   (case parse_expr_list ts
	     of NONE => SOME ([e],ts)
	      | SOME (es,ts) => SOME (e::es,ts)))

  and parse_sym_list ts = 
      (case expect_SYM ts
	of NONE => NONE
	 | SOME (s,ts) => 
	   (case parse_sym_list ts
	     of NONE => SOME ([s],ts)
	      | SOME (ss,ts) => SOME (s::ss,ts)))


  fun parseExpr str = 
      (case parse_expr (lexString str)
	of SOME (e,[]) => e
	 | SOME (_,_) => parseError "leftover characters past parsed expression"
	 | NONE => parseError "cannot parse expression")


  fun parseDecl str = 
      (case parse_decl (lexString str)
	of SOME (d,[]) => d
	 | SOME (_,_) => parseError "leftover characters past parsed expression"
	 | NONE => parseError "cannot parse expression")
      

end
