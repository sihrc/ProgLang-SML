(* 
 *   CODE FOR HOMEWORK 4
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
   *   Details in lecture 5
   *
   *   Modified to deal with keywords correctly
   *
   *)

  datatype token = T_LET 
                 | T_IN
                 | T_SYM of string 
                 | T_INT of int 
                 | T_TRUE 
                 | T_FALSE
                 | T_EQUAL
		 | T_LESS
                 | T_IF 
                 | T_THEN
                 | T_ELSE
                 | T_LPAREN 
                 | T_RPAREN
                 | T_PLUS
                 | T_MINUS
                 | T_TIMES
		 | T_BACKSLASH
 		 | T_RARROW
 		 | T_LARROW
 		 | T_DCOLON
                 | T_COMMA
                 | T_LBRACKET
                 | T_RBRACKET
		 | T_LBRACE
		 | T_RBRACE
		 | T_DOT
		 | T_HASH
		 | T_DDOTS
		 | T_BAR
		 | T_MATCH
		 | T_WITH


  fun stringOfToken T_LET = "T_LET"
    | stringOfToken T_IN = "T_IN"
    | stringOfToken (T_SYM s) = "T_SYM["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken T_TRUE = "T_TRUE"
    | stringOfToken T_FALSE = "T_FALSE"
    | stringOfToken T_EQUAL = "T_EQUAL"
    | stringOfToken T_LESS = "T_LESS"
    | stringOfToken T_IF  = "T_IF"
    | stringOfToken T_THEN  = "T_THEN"
    | stringOfToken T_ELSE  = "T_ELSE"
    | stringOfToken T_LPAREN = "T_LPAREN"
    | stringOfToken T_RPAREN = "T_RPAREN"
    | stringOfToken T_PLUS = "T_PLUS"
    | stringOfToken T_MINUS = "T_MINUS"
    | stringOfToken T_TIMES = "T_TIMES"
    | stringOfToken T_BACKSLASH = "T_BACKSLASH"
    | stringOfToken T_RARROW = "T_RARROW"
    | stringOfToken T_LARROW = "T_LARROW"
    | stringOfToken T_DCOLON = "T_DCOLON"
    | stringOfToken T_COMMA = "T_COMMA"
    | stringOfToken T_LBRACKET = "T_LBRACKET"
    | stringOfToken T_RBRACKET = "T_RBRACKET"
    | stringOfToken T_LBRACE = "T_LBRACE"
    | stringOfToken T_RBRACE = "T_RBRACE"
    | stringOfToken T_DOT = "T_DOT"
    | stringOfToken T_HASH = "T_HASH"
    | stringOfToken T_DDOTS = "T_DDOTS"
    | stringOfToken T_BAR = "T_BAR"
    | stringOfToken T_MATCH = "T_MATCH"
    | stringOfToken T_WITH = "T_WITH"

                   
  fun whitespace _ = NONE
                     
  fun produceSymbol "let" = SOME (T_LET)
    | produceSymbol "in" = SOME (T_IN)
    | produceSymbol "true" = SOME (T_TRUE)
    | produceSymbol "false" = SOME (T_FALSE)
    | produceSymbol "if" = SOME (T_IF)
    | produceSymbol "then" = SOME (T_THEN)
    | produceSymbol "else" = SOME (T_ELSE)
    | produceSymbol "match" = SOME (T_MATCH)
    | produceSymbol "with" = SOME (T_WITH)
    | produceSymbol text = SOME (T_SYM text)
                           
  fun produceInt text = (case Int.fromString text
                          of NONE => parseError "integer literal out of bounds"
                           | SOME i => SOME (T_INT i))
                        
  fun produceEqual _ = SOME (T_EQUAL)
  fun produceLess _ = SOME (T_LESS)
  fun produceLParen _ = SOME (T_LPAREN)
  fun produceRParen _ = SOME (T_RPAREN)

  fun producePlus _ = SOME (T_PLUS)
  fun produceMinus _ = SOME (T_MINUS)
  fun produceTimes _ = SOME (T_TIMES)
  fun produceComma _ = SOME (T_COMMA)

  fun produceBackslash _ = SOME (T_BACKSLASH)
  fun produceDColon _ = SOME (T_DCOLON)

  fun produceLArrow _ = SOME (T_LARROW)
  fun produceRArrow _ = SOME (T_RARROW)

  fun produceLBracket _ = SOME (T_LBRACKET)
  fun produceRBracket _ = SOME (T_RBRACKET)
  fun produceLBrace _ = SOME (T_LBRACE)
  fun produceRBrace _ = SOME (T_RBRACE)
  fun produceDDots _ = SOME (T_DDOTS)
  fun produceDot _ = SOME (T_DOT)
  fun produceHash _ = SOME (T_HASH)
  fun produceBar _ = SOME (T_BAR)
                       
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",         whitespace),
                 ("=",                    produceEqual),
                 ("\\+",                  producePlus),
		 ("\\*",                  produceTimes),
		 ("\\\\",                 produceBackslash),
		 ("->",                   produceRArrow),
		 ("<-",                   produceLArrow),
		 ("<",                    produceLess),
                 ("-",                    produceMinus),
		 ("#",                    produceHash),
                 ("::",                   produceDColon),
                 ("\\.\\.",               produceDDots),
		 ("\\.",                  produceDot),
		 (",",                    produceComma),
		 ("\\|",                  produceBar),
                 ("[a-zA-Z][a-zA-Z0-9]*", produceSymbol),
                 ("~?[0-9]+",             produceInt),
                 ("\\(",                  produceLParen),
                 ("\\)",                  produceRParen),
                 ("{",                  produceLBrace),
                 ("}",                  produceRBrace),
                 ("\\[",                  produceLBracket),
                 ("\\]",                  produceRBracket)]
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
   *   A SIMPLE PARSER FOR AN ML-LIKE SYNTAX
   * 
   *
   *   expr ::= eterm T_EQUAL eterm        
   *            eterm T_LESS eterm         
   *            eterm                      
   *
   *   eterm ::= cterm T_DCOLON cterm      
   *             cterm                     
   *
   *   cterm ::= term T_PLUS term         
   *             term T_MINUS term        
   *             term                     
   *
   *   term :: = aterm aterm_list        
   *            
   *   aterm_list ::= aterm aterm_list                [aterm_list_ATERM_LIST]
   *                  <empty>                         [aterm_list_EMPTY]
   *   expr_list  ::= expr T_COMMA expr_list
   *                  expr
   *                  <empty>
   *   fields ::= T_SYM T_EQUAL expr T_COMMA fields
                  T_SYM T_EQUAL expr
                  <empty>

   *   aterm ::= T_INT                                                              [aterm_INT]
   *             T_TRUE                                                             [aterm_TRUE]
   *             T_FALSE                                                            [aterm_FALSE]
   *             T_SYM                                                              [aterm_SYM]
   *             T_HASH T_SYM expr                                                  [aterm_FIELD]
   *             T_LBRACKET expr_list T_RBRACKET                                    [aterm_LIST] 
   *             T_LBRACE fields T_RBRACE                                           [aterm_RECORD]
   *             T_BACKSLASH T_SYM T_RARROW expr                                    [aterm_FUN]
   *             T_LPAREN expr T_RPAREN                                             [aterm_PARENS]
   *             T_LBRACKET expr T_DDOTS expr T_RBRACKET                            [aterm_INTERVAL]
   *             T_IF expr T_THEN expr T_ELSE expr                                  [aterm_IF]
   *             T_LET T_SYM T_EQUAL expr T_IN expr                                 [aterm_LET]
   *             T_LBRACKET expr T_BAR T_SYM T_LARROW expr T_RBRACKET               [aterm_MAP]
   *             T_LBRACKET expr T_BAR T_SYM T_LARROW expr T_COMMA expr T_RBRACKET  [aterm_FILTER]
   *             T_LET T_SYM sym_list T_EQUAL expr T_IN expr                        [aterm_LET_FUN]

   *             T_MATCH expr T_WITH T_LBRACKET T_RBRACKET T_RARROW  [aterm_MATCH]
                      expr T_BAR T_SYM T_DCOLON T_SYM T_RARROW expr

   *)


  fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
    | expect_INT _ = NONE

  fun expect_SYM ((T_SYM s)::ts) = SOME (s,ts)
    | expect_SYM _ = NONE

  (*   expect tokens ts 
   *   checks if ts starts with tokens specified
   *   this can only be used for tokens that do not parse to a value 
   *)

  fun expect [] ts = SOME ts
    | expect (token::tokens) (t::ts) = if token = t then 
					 expect tokens ts
				       else NONE
    | expect _ _ = NONE


  fun expect_LET ts = expect [T_LET] ts
  fun expect_IN ts = expect [T_IN] ts
  fun expect_TRUE ts = expect [T_TRUE] ts
  fun expect_FALSE ts = expect [T_FALSE] ts
  fun expect_EQUAL ts = expect [T_EQUAL] ts
  fun expect_LESS ts = expect [T_LESS] ts
  fun expect_IF ts = expect [T_IF ] ts
  fun expect_THEN ts = expect [T_THEN] ts
  fun expect_ELSE ts = expect [T_ELSE] ts
  fun expect_LPAREN ts = expect [T_LPAREN ] ts
  fun expect_RPAREN ts = expect [T_RPAREN] ts
  fun expect_PLUS ts = expect [T_PLUS] ts
  fun expect_MINUS ts = expect [T_MINUS] ts
  fun expect_TIMES ts = expect [T_TIMES] ts
  fun expect_BACKSLASH ts = expect [T_BACKSLASH] ts
  fun expect_RARROW ts = expect [T_RARROW] ts
  fun expect_LARROW ts = expect [T_LARROW] ts
  fun expect_DCOLON ts = expect [T_DCOLON] ts
  fun expect_COMMA ts = expect [T_COMMA] ts
  fun expect_LBRACKET ts = expect [T_LBRACKET] ts
  fun expect_RBRACKET ts = expect [T_RBRACKET] ts
  fun expect_LBRACE ts = expect [T_LBRACE] ts
  fun expect_RBRACE ts = expect [T_RBRACE] ts
  fun expect_DOT ts = expect [T_DOT] ts
  fun expect_HASH ts = expect [T_HASH] ts
  fun expect_DDOTS ts = expect [T_DDOTS] ts
  fun expect_BAR ts = expect [T_BAR] ts
  fun expect_MATCH ts = expect [T_MATCH] ts
  fun expect_WITH ts = expect [T_WITH] ts
			  


  fun choose [] ts = NONE
    | choose (parser::parsers) ts = 
      (case parser ts
	of NONE => choose parsers ts
	 | s => s)


  (*
   *  some helper functions to construct function calls in the internal representation
   *)

  fun call1 oper e1 = I.EApp (I.EIdent oper, e1)

  fun call2 oper e1 e2 = I.EApp (I.EApp (I.EIdent oper, e1), e2)



  fun parse_expr ts = 
      (case parse_eterm ts
	of NONE => NONE
	 | SOME (e1,ts) => 
	   (case expect_EQUAL ts
	     of NONE => (case expect_LESS ts
			  of NONE => SOME (e1,ts)
			   | SOME ts => 
			     (case parse_eterm ts
			       of NONE => NONE
				| SOME (e2,ts) => SOME (call2 "less" e1 e2, ts)))
	      | SOME ts => 
		(case parse_eterm ts
		  of NONE => NONE
		   | SOME (e2,ts) => SOME (call2 "equal" e1 e2, ts))))


  and parse_eterm ts = 
      (case parse_cterm ts
	of NONE => NONE
	 | SOME (e1,ts) => 
	   (case expect_DCOLON ts
	     of NONE => SOME (e1,ts)
	      | SOME ts => 
		(case parse_cterm ts
		  of NONE => NONE
		   | SOME (e2,ts) => SOME (call2 "cons" e1 e2, ts))))


  and parse_cterm ts = 
      (case parse_term ts
	of NONE => NONE
	 | SOME (e1,ts) => 
	   (case expect_PLUS ts
	     of NONE =>
		(case expect_MINUS ts
		  of NONE => SOME (e1,ts)
		   | SOME ts => 
		     (case parse_term ts
		       of NONE => NONE
			| SOME (e2,ts) => SOME (call2 "sub" e1 e2, ts)))
	      | SOME ts => 
		(case parse_term ts
		  of NONE => NONE
		   | SOME (e2,ts) => SOME (call2 "add" e1 e2, ts))))


  and parse_term ts = let
    fun convert [] = parseError "empty list of aterms"
      | convert [at] = at
      | convert (at1::at2::ats) = convert ((I.EApp (at1,at2))::ats)
  in
    (case parse_aterm ts
       of NONE => NONE
        | SOME (at,ts) => 
          (case parse_aterm_list ts
             of NONE => NONE
              | SOME (ats,ts) => SOME (convert (at::ats),ts)))
  end
   

  and parse_aterm ts = 
      choose [parse_aterm_INT,
              parse_aterm_TRUE,
              parse_aterm_FALSE,
              parse_aterm_SYM,
	      parse_aterm_FUN,
              parse_aterm_PARENS,
	      parse_aterm_IF,
	      parse_aterm_LET,
	      parse_aterm_LET_FUN,
        parse_aterm_MATCH,
        parse_aterm_LIST,
        parse_aterm_INTERVAL,
        parse_aterm_MAP,
        parse_aterm_FILTER,
        parse_aterm_RECORD,
        parse_aterm_FIELD
	     ] ts

  and parse_aterm_INT ts = 
    (case expect_INT ts 
      of NONE => NONE
       | SOME (i,ts) => SOME (I.EVal (I.VInt i),ts))

  and parse_aterm_TRUE ts = 
    (case expect_TRUE ts
      of NONE => NONE
       | SOME ts => SOME (I.EVal (I.VBool true),ts))

  and parse_aterm_FALSE ts = 
    (case expect_FALSE ts
      of NONE => NONE
       | SOME ts => SOME (I.EVal (I.VBool false),ts))

  and parse_aterm_SYM ts = 
    (case expect_SYM ts
      of NONE => NONE
       | SOME (s,ts) => SOME (I.EIdent s,ts))

  and parse_aterm_FUN ts = 
    (case expect_BACKSLASH ts 
      of NONE => NONE
       | SOME ts => 
	 (case expect_SYM ts
	   of NONE => NONE
	    | SOME (s,ts) => 
	      (case expect_RARROW ts
		of NONE => NONE
		 | SOME ts => 
		   (case parse_expr ts
		     of NONE => NONE
		      | SOME (e,ts) => SOME (I.EFun (s,e), ts)))))

  and parse_aterm_PARENS ts = 
    (case expect_LPAREN ts
      of NONE => NONE
       | SOME ts =>
         (case parse_expr ts
           of NONE => NONE
            | SOME (e,ts) => 
              (case expect_RPAREN ts
                of NONE => NONE
                | SOME ts => SOME (e,ts))))

  and parse_aterm_IF ts = 
    (case expect_IF ts
      of NONE => NONE
       | SOME ts => 
         (case parse_expr ts
           of NONE => NONE
            | SOME (e1,ts) => 
              (case expect_THEN ts
                of NONE => NONE
                 | SOME ts => 
                   (case parse_expr ts
                     of NONE => NONE
                      | SOME (e2,ts) => 
                        (case expect_ELSE ts
                          of NONE => NONE
                           | SOME ts => 
                             (case parse_expr ts
                               of NONE => NONE
                                | SOME (e3,ts) => SOME (I.EIf (e1,e2,e3),ts)))))))

  and parse_aterm_MATCH ts = 
    (case expect_MATCH ts
      of NONE => NONE
       | SOME ts => 
         (case parse_expr ts
           of NONE => NONE
            | SOME (e1,ts) => 
              (case expect_WITH ts 
                of NONE => NONE
                |  SOME ts => 
                (case expect_LBRACKET ts
                  of NONE => NONE
                   | SOME ts => 
                     (case expect_RBRACKET ts
                       of NONE => NONE
                        | SOME ts => 
                            (case expect_RARROW ts
                              of NONE => NONE
                               | SOME ts => 
                                 (case parse_expr ts
                                   of NONE => NONE
                                    | SOME (e2,ts) => 
                                    (case expect_BAR ts 
                                      of NONE => NONE
                                       | SOME (ts) => 
                                       (case expect_SYM ts 
                                        of NONE => NONE
                                        |  SOME (sym1, ts) => 
                                        (case expect_DCOLON ts 
                                          of NONE => NONE
                                          |  SOME ts => 
                                          (case expect_SYM ts 
                                            of NONE => NONE
                                            |  SOME (sym2, ts) =>
                                            (case expect_RARROW ts 
                                              of NONE => NONE
                                              |  SOME ts => 
                                              (case parse_expr ts 
                                                of NONE => NONE
                                                |  SOME (e3, ts) => SOME ((createMatch e1 e2 e3 sym1 sym2), ts))))))))))))))

  and parse_aterm_MAP ts = 
     (case expect_LBRACKET ts
      of NONE => NONE
       | SOME ts => 
         (case parse_expr ts
           of NONE => NONE
            | SOME (e1,ts) => 
              (case expect_BAR ts 
                of NONE => NONE
                |  SOME ts => 
                (case expect_SYM ts
                  of NONE => NONE
                   | SOME (s1,ts) => 
                     (case expect_LARROW ts
                       of NONE => NONE
                        | SOME ts => 
                            (case parse_expr ts
                              of NONE => NONE
                               | SOME (e2,ts) => 
                                 (case expect_RBRACKET ts
                                   of NONE => NONE
                                    |SOME ts => SOME(
                                      (I.EApp(
                                        I.EApp(
                                          (I.EIdent "map"), 
                                          (I.EFun(s1, e1))
                                          ),
                                         e2))
                                      ,ts))))))))

  and parse_aterm_FILTER ts = 
     (case expect_LBRACKET ts
      of NONE => NONE
       | SOME ts => 
         (case parse_expr ts
           of NONE => NONE
            | SOME (e1,ts) => 
              (case expect_BAR ts 
                of NONE => NONE
                |  SOME ts => 
                (case expect_SYM ts
                  of NONE => NONE
                   | SOME (s1,ts) => 
                     (case expect_LARROW ts
                       of NONE => NONE
                        | SOME ts => 
                            (case parse_expr ts
                              of NONE => NONE
                               | SOME (e2,ts) => 
                                  (case expect_COMMA ts
                                    of NONE => NONE
                                     | SOME ts => 
                                       (case parse_expr ts
                                         of NONE => NONE
                                          | SOME (e3,ts) => 
                                           (case expect_RBRACKET ts
                                             of NONE => NONE
                                              |SOME ts => SOME(
                                                (I.EApp(
                                                  I.EApp(
                                                    (I.EIdent "map"), 
                                                    (I.EFun(s1, e1))
                                                    ),
                                                   I.EApp(
                                                    I.EApp(
                                                      (I.EIdent "filter"),
                                                      (I.EFun(s1, e3))
                                                      ),e2
                                                    )
                                                   ))
                                                ,ts))))))))))



  and createMatch e1 e2 e3 sym1 sym2 =
      I.EIf (
      I.EApp (I.EApp (I.EIdent "equal", e1),I.EVal (I.VList [])),
      e2,
      I.ELet (sym1,I.EApp (I.EIdent "hd", e1),
      I.ELet (sym2,I.EApp
      (I.EIdent "tl", e1), e3))
      )

  and parse_aterm_LET ts = 
    (case expect_LET ts 
      of NONE => NONE
       | SOME ts => 
         (case expect_SYM ts 
           of NONE => NONE
            | SOME (s,ts) => 
              (case expect_EQUAL ts
                of NONE => NONE
                 | SOME ts => 
                   (case parse_expr ts
                     of NONE => NONE
                      | SOME (e1,ts) => 
                        (case expect_IN ts
                          of NONE => NONE
                           | SOME ts => 
                             (case parse_expr ts
                               of NONE => NONE
                                | SOME (e2,ts) => SOME (I.ELet (s,e1,e2),ts)))))))

  and parse_aterm_LET_FUN ts = 
    (case expect_LET ts 
      of NONE => NONE
       | SOME ts => 
         (case expect_SYM ts 
           of NONE => NONE
            | SOME (s,ts) => 
	        (case parse_sym_list ts
                of NONE => NONE
                 | SOME (symlist,ts) => 
                   (case expect_EQUAL ts
                     of NONE => NONE
                      | SOME ts => 
                        (case parse_expr ts
                          of NONE => NONE
                           | SOME (e1,ts) => 
                             (case expect_IN ts
                               of NONE => NONE
                                | SOME ts => 
                                  (case parse_expr ts
                                    of NONE => NONE
                                     | SOME (e2,ts) => 
                                        make_recur_ELetFun s symlist e1 e2 ts)))))))
  and make_recur_ELetFun (s:string) ((I.EIdent symhd)::symtl) (e1:I.expr) (e2:I.expr) ts = let
    fun efun_rec [] = e1
      | efun_rec ((I.EIdent s)::sx) = I.EFun(s, (efun_rec sx))
      | efun_rec _ = parseError "Efun_rec error"
  in
    SOME ((I.ELetFun(s, symhd, (efun_rec symtl), e2)), ts)
  end
    | make_recur_ELetFun _ _ _ _ ts = parseError "Make_recur_EletFun"


  and parse_sym_list ts = 
    choose [parse_sym_list_SYM_LIST,
            parse_sym_list_EMPTY] ts

  and parse_sym_list_SYM_LIST ts = 
    (case parse_aterm_SYM ts
      of NONE => NONE
       | SOME (at,ts) => 
         (case parse_sym_list ts
           of NONE => NONE
            | SOME (ats,ts) => SOME (at::ats,ts)))

  and parse_sym_list_EMPTY ts = SOME ([], ts)

  and parse_aterm_list ts = 
      choose [parse_aterm_list_ATERM_LIST,
	      parse_aterm_list_EMPTY
	     ] ts 

  and parse_aterm_list_ATERM_LIST ts = 
    (case parse_aterm ts
      of NONE => NONE
       | SOME (at,ts) => 
         (case parse_aterm_list ts
           of NONE => NONE
            | SOME (ats,ts) => SOME (at::ats,ts)))

  and parse_aterm_list_EMPTY ts = SOME ([], ts)

  and parse_aterm_LIST ts = (case expect_LBRACKET ts 
    of NONE => NONE
     | SOME ts => (case parse_expr_list ts 
      of NONE => NONE
      |SOME (es, ts) => (case expect_RBRACKET ts 
        of NONE => NONE
         | SOME ts => SOME ((createCons es),ts))))

  and createCons es = List.foldr (fn (e, es') => I.EApp (I.EApp (I.EIdent "cons", e), es')) (I.EVal (I.VList [])) es

  and parse_expr_list ts =
    (case parse_expr ts 
      of NONE => SOME ([], ts)
       | SOME (e, ts) =>
        (case expect_COMMA ts
          of NONE => SOME ([e], ts)
          | SOME(ts) => case parse_expr_list ts 
            of NONE => NONE
            | SOME(es, ts) => SOME (e::es, ts)))

  and parse_aterm_INTERVAL ts = 
    (case expect_LBRACKET ts 
      of NONE => NONE
      |  SOME ts => 
        (case parse_expr ts 
          of NONE => NONE
            |SOME (e1,ts) => 
              (case expect_DDOTS ts 
                of NONE => NONE
                 | SOME ts => 
                  (case parse_expr ts 
                    of NONE => NONE
                     | SOME (e2, ts) => 
                       (case expect_RBRACKET ts 
                        of NONE => NONE
                         | SOME ts =>  SOME((I.EApp (I.EApp (I.EIdent "interval", e1), e2)),ts))))))
  and parse_fields ts =
    (case expect_SYM ts 
      of NONE => SOME ([], ts)
       | SOME (sym1, ts) =>
        (case expect_EQUAL ts 
          of NONE => NONE
          |  SOME ts =>
            (case parse_expr ts 
              of NONE=>NONE
              |  SOME (expr,ts) => 
                (case expect_COMMA ts
                  of NONE => SOME ([(sym1, expr)], ts)
                  | SOME(ts) => case parse_fields ts 
                    of NONE => NONE
                    | SOME(es, ts) => SOME ((sym1, expr)::es, ts)))))

  and parse_aterm_FIELD ts = 
    (case expect_HASH ts 
      of NONE => NONE
        |SOME ts => 
          (case expect_SYM ts 
            of NONE => NONE
              |SOME (s1,ts) => 
                (case parse_expr ts 
                  of NONE => NONE
                    |SOME (expr,ts) => SOME((I.EField (expr,s1)),ts))))

  and parse_aterm_RECORD ts = 
    (case expect_LBRACE ts 
      of NONE => NONE
      |  SOME (ts) => 
      (case parse_fields ts 
        of NONE => NONE
        |  SOME (expr, ts) => 
        (case expect_RBRACE ts
          of NONE => NONE
          |  SOME ts => SOME ((I.ERecord expr),ts))))
  fun parse ts = 
      (case parse_expr ts
        of SOME (e,[]) => e
         | SOME (_,_)  => parseError "leftover characters past parsed expression"
         | NONE => parseError "cannot parse input")
      

end
