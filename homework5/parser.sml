(* 
 *   CODE FOR HOMEWORK 5
 *)



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
                 | T_IN
                 | T_SYM of string 
                 | T_INT of int 
                 | T_TRUE 
                 | T_FALSE
                 | T_EQUAL
                 | T_IF 
		 | T_THEN
                 | T_ELSE
                 | T_LPAREN 
                 | T_RPAREN
                 | T_PLUS
                 | T_TIMES
                 | T_COMMA
		 | T_SEMICOLON
		 | T_WHILE
		 | T_PRINT
		 | T_LBRACE
		 | T_RBRACE
		 | T_FUNCTION
		 | T_PROCEDURE
		 | T_VAR
		 | T_CONST
		 | T_LEFTARROW
		 | T_FOR
		 | T_LETVAR


  fun stringOfToken T_LET = "T_LET"
    | stringOfToken T_IN = "T_IN"
    | stringOfToken (T_SYM s) = "T_SYM["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken T_TRUE = "T_TRUE"
    | stringOfToken T_FALSE = "T_FALSE"
    | stringOfToken T_EQUAL = "T_EQUAL"
    | stringOfToken T_IF  = "T_IF"
    | stringOfToken T_THEN  = "T_THEN"
    | stringOfToken T_ELSE  = "T_ELSE"
    | stringOfToken T_LPAREN = "T_LPAREN"
    | stringOfToken T_RPAREN = "T_RPAREN"
    | stringOfToken T_PLUS = "T_PLUS"
    | stringOfToken T_TIMES = "T_TIMES"
    | stringOfToken T_COMMA = "T_COMMA"
    | stringOfToken T_SEMICOLON = "T_SEMICOLON"
    | stringOfToken T_WHILE = "T_WHILE"
    | stringOfToken T_PRINT = "T_PRINT"
    | stringOfToken T_LBRACE = "T_LBRACE"
    | stringOfToken T_RBRACE = "T_RBRACE"
    | stringOfToken T_FUNCTION = "T_FUNCTION"
    | stringOfToken T_PROCEDURE = "T_PROCEDURE"
    | stringOfToken T_VAR = "T_VAR"
    | stringOfToken T_CONST = "T_CONST"
    | stringOfToken T_LEFTARROW = "T_LEFTARROW"
    | stringOfToken T_FOR = "T_FOR"
    | stringOfToken T_LETVAR = "T_LETVAR"

                   
  fun whitespace _ = NONE
                     
  fun produceSymbol "let" = SOME (T_LET)
    | produceSymbol "in" = SOME (T_IN)
    | produceSymbol "true" = SOME (T_TRUE)
    | produceSymbol "false" = SOME (T_FALSE)
    | produceSymbol "if" = SOME (T_IF)
    | produceSymbol "then" = SOME (T_THEN)
    | produceSymbol "else" = SOME (T_ELSE)
    | produceSymbol "while" = SOME (T_WHILE)
    | produceSymbol "print" = SOME (T_PRINT)
    | produceSymbol "function" = SOME (T_FUNCTION)
    | produceSymbol "procedure" = SOME (T_PROCEDURE)
    | produceSymbol "var" = SOME (T_VAR)
    | produceSymbol "const" = SOME (T_CONST)
    | produceSymbol "for" = SOME (T_FOR)
    | produceSymbol "letvar" = SOME (T_LETVAR)
    | produceSymbol text = SOME (T_SYM text)
                           
  fun produceInt text = (case Int.fromString text
                          of NONE => parseError "integer literal out of bounds"
                           | SOME i => SOME (T_INT i))
                        
  fun produceEqual _ = SOME (T_EQUAL)
  fun produceLParen _ = SOME (T_LPAREN)
  fun produceRParen _ = SOME (T_RPAREN)

  fun produceLBrace _ = SOME (T_LBRACE)
  fun produceRBrace  _ = SOME (T_RBRACE)

  fun producePlus _ = SOME (T_PLUS)
  fun produceTimes _ = SOME (T_TIMES)
  fun produceComma _ = SOME (T_COMMA)

  fun produceLeftArrow _ = SOME (T_LEFTARROW)

  fun produceSemiColon _ = SOME (T_SEMICOLON)
                       
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",         whitespace),
                 ("=",                    produceEqual),
                 ("\\+",                  producePlus),
		 ("\\*",                  produceTimes),
		 (",",                    produceComma),
		 (";",                    produceSemiColon),
		 ("<-",                   produceLeftArrow),
                 ("[a-zA-Z][a-zA-Z0-9]*", produceSymbol),
                 ("~?[0-9]+",             produceInt),
                 ("\\(",                  produceLParen),
                 ("\\)",                  produceRParen),
                 ("{",                    produceLBrace),
                 ("}",                    produceRBrace)]
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
   *   A SIMPLE PARSER FOR A C-LIKE SYNTAX 
   * 
   *   Grammar:
   * 
   *   stmt ::= T_IF expr stmt T_ELSE stmt
   *            T_IF expr stmt
   *            T_WHILE expr stmt
   *            T_SYM T_LEFTARROW expr
   *            T_SYM T_LPAREN expr_list T_RPAREN
   *            T_PRINT T_LPAREN expr_list T_RPAREN
   *            T_LBRACE T_RBRACE
   *            T_LBRACE stmt_list T_RBRACE
   *
   *   stmt_list ::= stmt T_SEMICOLON stmt_list
   *                 stmt
   * 
   *   expr ::= eterm T_EQUAL term       
   *            eterm                    
   *
   *   eterm ::= term T_PLUS term        
   *             term                    
   *            
   *   term ::= T_INT                            
   *            T_TRUE                           
   *            T_FALSE                          
   *            T_SYM T_LPAREN expr_list T_RPARE
   *            T_SYM                               
   *            T_LPAREN expr T_RPAREN              
   *            T_IF expr T_THEN expr T_ELSE expr
   *            T_LET T_SYM T_EQUAL expr T_IN expr  
   * 
   *   expr_list ::= expr T_COMMA expr_list               
   *                 expr
   *                 <empty>                       
   *
   *   decl ::= function T_SYM T_LPAREN sym_list T_RPAREN expr
   *            procedure T_SYM T_LPAREN sym_list T_RPAREN stmt
   *            var T_SYM T_EQUAL expr
   *            const T_SYM T_EQUAL expr
   *            stmt
   *
   *   sym_list ::= T_SYM T_COMMA sym_list
   *                T_SYM
   *                <empty>
   *
   *)


  datatype decl = DeclFunc of (string * string list * I.expr)
		| DeclProc of (string * string list * I.stmt)
		| DeclStmt of I.stmt
		| DeclVar of string * I.expr
		| DeclConst of string * I.expr

  fun expect token (t::ts) = if t=token then SOME ts else NONE
    | expect _ _ = NONE

  fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
    | expect_INT _ = NONE

  fun expect_SYM ((T_SYM s)::ts) = SOME (s,ts)
    | expect_SYM _ = NONE

  fun choose [] ts = NONE
    | choose (p::ps) ts = 
        (case p ts
	  of NONE => choose ps ts
	   | s => s)

  fun parse_decl ts = let
    fun decl_var ts = 
	(case expect T_VAR ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_SYM ts
	       of NONE => NONE
		| SOME (n,ts) =>
		  (case expect T_EQUAL ts
		    of NONE => NONE
		     | SOME ts => 
		       (case parse_expr ts 
			 of NONE => NONE
			  | SOME (e,ts) => SOME (DeclVar (n,e),ts)))))
    fun decl_const ts = 
	(case expect T_CONST ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_SYM ts
	       of NONE => NONE
		| SOME (n,ts) =>
		  (case expect T_EQUAL ts
		    of NONE => NONE
		     | SOME ts => 
		       (case parse_expr ts 
			 of NONE => NONE
			  | SOME (e,ts) => SOME (DeclConst (n,e),ts)))))
    fun decl_func ts = 
	(case expect T_FUNCTION ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_SYM ts
	       of NONE => NONE
		| SOME (n,ts) => 
		  (case expect T_LPAREN ts
		    of NONE => NONE
		     | SOME ts =>
		       (case parse_sym_list ts
			 of NONE => NONE
			  | SOME (ps,ts) => 
			    (case expect T_RPAREN ts
			      of NONE => NONE
			       | SOME ts =>
				 (case parse_expr ts
				   of NONE => NONE
				    | SOME (e,ts) => 
				        SOME (DeclFunc (n,ps,e),ts)))))))
    fun decl_proc ts = 
	(case expect T_PROCEDURE ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_SYM ts
	       of NONE => NONE
		| SOME (n,ts) => 
		  (case expect T_LPAREN ts
		    of NONE => NONE
		     | SOME ts =>
		       (case parse_sym_list ts
			 of NONE => NONE
			  | SOME (ps,ts) => 
			    (case expect T_RPAREN ts
			      of NONE => NONE
			       | SOME ts =>
				 (case parse_stmt ts
				   of NONE => NONE
				    | SOME (s,ts) => 
				        SOME (DeclProc (n,ps,s),ts)))))))
    fun decl_stmt ts = 
	(case parse_stmt ts
	  of NONE => NONE
	   | SOME (s,ts) => SOME (DeclStmt s,ts))
  in
    choose [decl_var, decl_const, decl_func, decl_proc, decl_stmt] ts
  end


  and parse_sym_list ts = 
      (case expect_SYM ts
	of NONE => SOME ([],ts)
	 | SOME (s,ts) => 
	   (case expect T_COMMA ts
	     of NONE => SOME ([s],ts)
	      | SOME ts => 
		(case parse_sym_list ts
		  of NONE => NONE
		   | SOME (ss,ts) => SOME (s::ss,ts))))


  and parse_stmt ts = let
    fun stmt_IF ts = 
	(case expect T_IF ts
	  of NONE => NONE
	   | SOME ts =>
	     (case parse_expr ts
	       of NONE => NONE
		| SOME (e,ts) => 
		  (case parse_stmt ts
		    of NONE => NONE
		     | SOME (s1,ts) => 
		       (case expect T_ELSE ts
			 of NONE => SOME (I.SIf (e,s1,I.SBlock []),ts)
			  | SOME ts => 
			    (case parse_stmt ts
			      of NONE => NONE
			       | SOME (s2,ts) => 
				 SOME (I.SIf (e,s1,s2),ts))))))
    fun stmt_WHILE ts = 
	(case expect T_WHILE ts
	  of NONE => NONE
	   | SOME ts => 
	     (case parse_expr ts
	       of NONE => NONE
		| SOME (e,ts) => 
		  (case parse_stmt ts
		    of NONE => NONE
		     | SOME (s,ts) => 
		       SOME (I.SWhile (e,s),ts))))
    fun stmt_UPDATE ts = 
	(case expect_SYM ts
	  of NONE => NONE
	   | SOME (n,ts) => 
	     (case expect T_LEFTARROW ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr ts
		    of NONE => NONE
		     | SOME (e,ts) => SOME (I.SUpdate (n,e),ts))))
    fun stmt_CALL ts = 
	(case expect_SYM ts
	  of NONE => NONE
	   | SOME (s,ts) => 
	     (case expect T_LPAREN ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr_list ts
		    of NONE => NONE
		     | SOME (es,ts) => 
		       (case expect T_RPAREN ts
			 of NONE => NONE
			  | SOME ts => SOME (I.SCall (s,es),ts)))))
    fun stmt_PRINT ts = 
	(case expect T_PRINT ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect T_LPAREN ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr_list ts
		    of NONE => NONE
		     | SOME (es,ts) => 
		       (case expect T_RPAREN ts
			 of NONE => NONE
			  | SOME ts => SOME (I.SPrint es,ts)))))
    fun stmt_BLOCK_EMPTY ts = 
	(case expect T_LBRACE ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect T_RBRACE ts
	       of NONE => NONE
		| SOME ts => SOME (I.SBlock [],ts)))
    fun stmt_BLOCK ts = 
	(case expect T_LBRACE ts
	  of NONE => NONE
	   | SOME ts => 
	     (case parse_stmt_list ts
	       of NONE => NONE
		| SOME (ss,ts) => 
		  (case expect T_RBRACE ts
		    of NONE => NONE
		     | SOME ts => SOME (I.SBlock ss,ts))))
    fun stmt_HD_UPDATE ts = 
  (case parse_expr ts
    of NONE => NONE
     | SOME (e1, ts)  => 
       (case expect T_LEFTARROW ts
         of NONE => NONE
        | SOME ts => 
          (case parse_expr ts
            of NONE => NONE
            | SOME (e2, ts) => case e1 of 
                I.ECall ("hd", [lst] )  => 
                SOME (I.SCall ("updateHd", [lst, e2]), ts)
                )))

  in
    choose [stmt_HD_UPDATE , stmt_IF, stmt_WHILE, stmt_UPDATE, stmt_CALL, stmt_PRINT,
	    stmt_BLOCK_EMPTY, stmt_BLOCK] ts
  end


  and parse_stmt_list ts = 
      (case parse_stmt ts
	of NONE => NONE 
	 | SOME (s,ts) => 
	   (case expect T_SEMICOLON ts
	     of NONE => SOME ([s],ts)
	      | SOME ts => 
		(case parse_stmt_list ts
		  of NONE => NONE
		   | SOME (ss,ts) => SOME (s::ss,ts))))

 
  and parse_expr ts = let
    fun expr_EQUAL ts = 
	(case parse_eterm ts
	  of NONE => NONE
	   | SOME (e1,ts) => 
             (case expect T_EQUAL ts
               of NONE => NONE
		| SOME ts => 
		  (case parse_eterm ts
                    of NONE => NONE
                     | SOME (e2,ts) => SOME (I.ECall ("=", [e1,e2]),ts))))
    fun expr_ETERM ts = parse_eterm ts
  in
    choose [expr_EQUAL, expr_ETERM] ts
  end


  and parse_eterm ts = let
    fun eterm_PLUS ts = 
	(case parse_term ts
	  of NONE => NONE
	   | SOME (e1,ts) => 
             (case expect T_PLUS ts
               of NONE => NONE
		| SOME ts => 
		  (case parse_term ts
                    of NONE => NONE
                     | SOME (e2,ts) => SOME (I.ECall ("+", [e1,e2]),ts))))
    fun eterm_TERM ts = parse_term ts
  in
    choose [eterm_PLUS, eterm_TERM] ts
  end


  and parse_term ts = let
    fun term_INT ts = 
	(case expect_INT ts 
	  of NONE => NONE
	   | SOME (i,ts) => SOME (I.EVal (I.VInt i),ts))
    fun term_TRUE ts = 
	(case expect T_TRUE ts
	  of NONE => NONE
	   | SOME ts => SOME (I.EVal (I.VBool true),ts))
    fun term_FALSE ts = 
	(case expect T_FALSE ts
	  of NONE => NONE
	   | SOME ts => SOME (I.EVal (I.VBool false),ts))
    fun term_CALL ts = 
	(case expect_SYM ts 
	  of NONE => NONE
	   | SOME (s,ts) =>
	     (case expect T_LPAREN ts
	       of NONE => NONE
		| SOME ts => 
		  (case parse_expr_list ts
		    of NONE => NONE
		     | SOME (es,ts) => 
		       (case expect T_RPAREN ts
			 of NONE => NONE
			  | SOME ts => SOME (I.ECall (s,es),ts)))))
    fun term_SYM ts = 
	(case expect_SYM ts
	  of NONE => NONE
	   | SOME (s,ts) => SOME (I.EIdent s,ts))
    fun term_PARENS ts = 
	(case expect T_LPAREN ts
	  of NONE => NONE
	   | SOME ts =>
             (case parse_expr ts
               of NONE => NONE
		| SOME (e,ts) => 
		  (case expect T_RPAREN ts
                    of NONE => NONE
                     | SOME ts => SOME (e,ts))))
    and term_LET ts = 
	(case expect T_LET ts 
	  of NONE => NONE
	   | SOME ts => 
             (case expect_SYM ts 
               of NONE => NONE
		| SOME (s,ts) => 
		  (case expect T_EQUAL ts
                    of NONE => NONE
                     | SOME ts => 
                       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e1,ts) => 
                            (case expect T_IN ts
                              of NONE => NONE
                               | SOME ts => 
				 (case parse_expr ts
				   of NONE => NONE
                                    | SOME (e2,ts) => SOME (I.ELet (s,e1,e2),ts)))))))
    fun term_IF ts = 
	(case expect T_IF ts
	  of NONE => NONE
	   | SOME ts =>
	     (case parse_expr ts
	       of NONE => NONE
		| SOME (e1,ts) => 
		  (case expect T_THEN ts
		    of NONE => NONE 
		     | SOME ts => 
		       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e2,ts) => 
			    (case expect T_ELSE ts
			      of NONE => NONE
			       | SOME ts => 
				 (case parse_expr ts
				   of NONE => NONE
				    | SOME (e3,ts) => 
				        SOME (I.EIf (e1,e2,e3),ts)))))))

  in
    choose [term_INT, term_TRUE, term_FALSE,
	    term_CALL, term_SYM, term_PARENS, term_IF, term_LET] ts
  end


  and parse_expr_list ts = 
      (case parse_expr ts
	of NONE => SOME ([],ts)
	 | SOME (e,ts) => 
	   (case expect T_COMMA ts
	     of NONE => SOME ([e],ts)
	      | SOME ts => 
		(case parse_expr_list ts
		  of NONE => NONE
		   | SOME (es,ts) => SOME (e::es,ts))))


  fun parseStmt ts = 
      (case parse_stmt ts
        of SOME (s,[]) => s
         | SOME (_,_)  => parseError "leftover characters past parsed expression"
         | NONE => parseError "cannot parse input")

  fun parseDecl ts = 
      (case parse_decl ts
        of SOME (d,[]) => d
         | SOME (_,_)  => parseError "leftover characters past parsed expression"
         | NONE => parseError "cannot parse input")
      

end
