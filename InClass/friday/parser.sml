
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
   *   Details in lecture 5
   *
   *   Modified to deal with keywords correctly
   *
   *   There are tokens for all sample parsers
   *   Not all parsers use all tokens
   *)

  datatype token = T_LET 
		 | T_SYM of string 
		 | T_INT of int 
		 | T_TRUE 
		 | T_FALSE
		 | T_PLUS
		 | T_TIMES
		 | T_EQUAL
		 | T_IF 
		 | T_LPAREN 
		 | T_RPAREN
		 | T_CONS
		 | T_HEAD
		 | T_TAIL
		 | T_NIL


  fun stringOfToken T_LET = "T_LET"
    | stringOfToken (T_SYM s) = "T_SYM["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken T_TRUE = "T_TRUE"
    | stringOfToken T_FALSE = "T_FALSE"
    | stringOfToken T_PLUS = "T_PLUS"
    | stringOfToken T_TIMES = "T_TIMES"
    | stringOfToken T_EQUAL = "T_EQUAL"
    | stringOfToken T_IF  = "T_IF"
    | stringOfToken T_LPAREN = "T_LPAREN"
    | stringOfToken T_RPAREN = "T_RPAREN"
    | stringOfToken T_CONS = "T_CONS"
    | stringOfToken T_HEAD = "T_HEAD"
    | stringOfToken T_TAIL = "T_TAIL"
    | stringOfToken T_NIL = "T_NIL"
			       

		   
  fun whitespace _ = NONE
		     
  fun produceSymbol "let" = SOME (T_LET)
    | produceSymbol "true" = SOME (T_TRUE)
    | produceSymbol "false" = SOME (T_FALSE)
    | produceSymbol "if" = SOME (T_IF)
    | produceSymbol "cons" = SOME (T_CONS)
    | produceSymbol "head" = SOME (T_HEAD)
    | produceSymbol "tail" = SOME (T_TAIL)
    | produceSymbol "nil" = SOME (T_NIL)
    | produceSymbol text = SOME (T_SYM text)
			   
  fun produceInt text = (case Int.fromString text
			  of NONE => parseError "integer literal out of bounds"
			   | SOME i => SOME (T_INT i))
			
  fun producePlus _ = SOME (T_PLUS)
  fun produceTimes _ = SOME (T_TIMES)
  fun produceEqual _ = SOME (T_EQUAL)
  fun produceLParen _ = SOME (T_LPAREN)
  fun produceRParen _ = SOME (T_RPAREN)
		       
		       
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",         whitespace),
		 ("\\+" ,                 producePlus),
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
   *   expr ::= T_INT 
   *            T_TRUE
   *            T_FALSE
   *            T_SYM 
   *            T_LPAREN expr_seq
   *
   *   expr_seq :: = T_LET T_LPAREN T_LPAREN T_SYM expr T_RPAREN T_RPAREN expr T_RPAREN
   *                 T_IF expr expr expr T_RPAREN
   *                 T_PLUS expr expr T_RPAREN
   *                 T_TIMES expr expr T_RPAREN
   *                 T_SYM expr T_RPAREN
   *)
			   
			   
  fun expect_INT ((T_INT i)::ts) = (i,ts)
    | expect_INT _ = parseError "expect_INT"
		     
  fun expect_TRUE (T_TRUE::ts) = ts
    | expect_TRUE _ = parseError "expect_TRUE"
		      
  fun expect_FALSE (T_FALSE::ts) = ts
    | expect_FALSE _ = parseError "expect_FALSE"
		       
  fun expect_SYM ((T_SYM s)::ts) = (s,ts)
    | expect_SYM _ = parseError "expect_SYM"
		     
  fun expect_IF (T_IF::ts) = ts
    | expect_IF _ = parseError "expect_IF"
		    
  fun expect_LET (T_LET::ts) = ts
    | expect_LET _ = parseError "expect_LET"
		     
  fun expect_EQUAL (T_EQUAL::ts) = ts
    | expect_EQUAL _ = parseError "expect_EQUAL"
		       
  fun expect_LPAREN (T_LPAREN::ts) = ts
    | expect_LPAREN _ = parseError "expect_LPAREN"
			
  fun expect_RPAREN (T_RPAREN::ts) = ts
    | expect_RPAREN _ = parseError "expect_RPAREN"
			
  fun expect_PLUS (T_PLUS::ts) = ts
    | expect_PLUS _ = parseError "expect_PLUS"
		      
  fun expect_TIMES (T_TIMES::ts) = ts
    | expect_TIMES _ = parseError "expect_TIMES"

  fun expect_CONS (T_CONS::ts) = ts
    | expect_CONS _ = parseError "expect_CONS"

  fun expect_HEAD (T_HEAD::ts) = ts
    | expect_HEAD _ = parseError "expect_HEAD"

  fun expect_TAIL (T_TAIL::ts) = ts
    | expect_TAIL _ = parseError "expect_TAIL"

  fun expect_NIL (T_NIL::ts) = ts
    | expect_NIL _ = parseError "expect_NIL"
		       
		       
		       
  fun parse_expr [] = parseError "unexpected end of token sequence"
    | parse_expr ((T_INT i)::ts) = (I.EVal (I.VInt i),ts)
    | parse_expr (T_TRUE::ts) = (I.EVal (I.VBool true),ts)
    | parse_expr (T_FALSE::ts) = (I.EVal (I.VBool false),ts)
    | parse_expr ((T_SYM id)::ts) = (I.EIdent id, ts)
    | parse_expr (T_LPAREN::ts) = let
	val (e,ts) = parse_expr_seq ts
      in
	(e,ts)
      end

    | parse_expr (T_NIL::ts) = (I.EVal (I.VList []),ts)
    | parse_expr _ = parseError "unexpected token"



  and parse_expr_seq [] = parseError "unexpected end of token sequence"
    | parse_expr_seq (T_LET::ts) = let
	val ts = expect_LPAREN ts
	val ts = expect_LPAREN ts
	val (name,ts) = expect_SYM ts
	val (e1, ts) = parse_expr ts
	val ts = expect_RPAREN ts
	val ts = expect_RPAREN ts
	val (e2, ts) = parse_expr ts
	val ts = expect_RPAREN ts
      in
	(I.ELet (name,e1,e2),ts)
      end
    | parse_expr_seq (T_IF::ts) = let
	val (e1,ts) = parse_expr ts
	val (e2,ts) = parse_expr ts
	val (e3,ts) = parse_expr ts
	val ts = expect_RPAREN ts
      in
	(I.EIf (e1,e2,e3),ts)
      end
    | parse_expr_seq (T_PLUS::ts) = let
	val (e1,ts) = parse_expr ts
	val (e2,ts) = parse_expr ts
	val ts = expect_RPAREN ts
      in
	(I.EAdd (e1,e2),ts)
      end
    | parse_expr_seq (T_TIMES::ts) = let
	val (e1,ts) = parse_expr ts
	val (e2,ts) = parse_expr ts
	val ts = expect_RPAREN ts
      in
	(I.EMul (e1,e2),ts)
      end
    | parse_expr_seq (T_EQUAL::ts) = let
	val (e1,ts) = parse_expr ts
	val (e2,ts) = parse_expr ts
	val ts = expect_RPAREN ts
      in
	(I.EEq (e1,e2),ts)
      end
    | parse_expr_seq ((T_SYM name)::ts) = let
	val (e, ts) = parse_expr ts
	val ts = expect_RPAREN ts
      in
	(I.ECall (name,[e]),ts)
      (* the syntax forces us to use only one argument here *)
      end
    | parse_expr_seq (T_CONS::ts) = let
	val (e1,ts) = parse_expr ts
	val (e2,ts) = parse_expr ts
	val ts = expect_RPAREN ts
      in
	(I.ECons (e1,e2),ts)
      end
    | parse_expr_seq (T_HEAD::ts) = let
	val (e1,ts) = parse_expr ts
	val ts = expect_RPAREN ts
      in
	(I.EHead e1,ts)
      end
    | parse_expr_seq (T_TAIL::ts) = let
	val (e1,ts) = parse_expr ts
	val ts = expect_RPAREN ts
      in
	(I.ETail e1,ts)
      end
    | parse_expr_seq _ = parseError "unexpected token"

			 

  fun parse ts = 
      (case parse_expr ts
	of (e,[]) => e
	 | _ => parseError "leftover characters past parsed expression")
      

end
