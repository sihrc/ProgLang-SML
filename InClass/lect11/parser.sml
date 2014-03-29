
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

  datatype token = T_INT of int
                 | T_WORD of string
		 | T_COLON
		 | T_SEMI
		 | T_IF
		 | T_THEN
		 | T_ELSE


  fun stringOfToken (T_WORD s) = "T_WORD["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken (T_COLON) = "T_COLON"
    | stringOfToken (T_SEMI) = "T_SEMI"
    | stringOfToken (T_IF) = "T_IF"
    | stringOfToken (T_THEN) = "T_THEN"
    | stringOfToken (T_ELSE) = "T_ELSE"
                   
  fun whitespace _ = NONE
                     

  fun produceInt text = (case Int.fromString text
                          of NONE => parseError "integer literal out of bounds"
                           | SOME i => SOME (T_INT i))

  fun produceColon _ = SOME (T_COLON)
  fun produceSemi _ = SOME (T_SEMI)

  fun produceWord "if" = SOME (T_IF)
    | produceWord "then" = SOME (T_THEN)
    | produceWord "else" = SOME (T_ELSE)
    | produceWord text = SOME (T_WORD text)
                        
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",         whitespace),
		 (":",                    produceColon),
		 (";",                    produceSemi),
                 ("~?[0-9]+[^ \\n\\t0-9]+",  produceWord),
                 ("~?[0-9]+",             produceInt),
                 ("[^ \\n\\t]+",          produceWord)]
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
   *   A SIMPLE PARSER FOR A FORTH-LIKE LANGUAGE
   *
   *   Grammar:
   *
   *   decl ::= T_COLON T_WORD sentence
   *            sentence
   *
   *   sentence ::= T_IF sentence T_THEN sentence
   *                T_IF sentence T_ELSE sentence T_THEN sentence
   *                word sentence
   *                word
   *                <empty>
   *
   *   word ::= T_INT
   *            T_WORD
   *)


  datatype decl = DDef of string * I.sentence
                | DSent of I.sentence


  fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
    | expect_INT _ = NONE

  fun expect_WORD ((T_WORD s)::ts) = SOME (s,ts)
    | expect_WORD _ = NONE

  fun expect_COLON (T_COLON::ts) = SOME ts
    | expect_COLON _ = NONE

  fun expect_SEMI (T_SEMI::ts) = SOME ts
    | expect_SEMI _ = NONE

  fun expect_IF (T_IF::ts) = SOME ts
    | expect_IF _ = NONE

  fun expect_THEN (T_THEN::ts) = SOME ts
    | expect_THEN _ = NONE

  fun expect_ELSE (T_ELSE::ts) = SOME ts
    | expect_ELSE _ = NONE


  fun choose [] ts = NONE
    | choose (parser::parsers) ts = 
      (case parser ts
	of NONE => choose parsers ts
	 | s => s)


  fun parse_decl ts = let
    fun decl_def ts = 
	(case expect_COLON ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_WORD ts
	       of NONE => NONE
		| SOME (w,ts) => 
		  (case parse_sentence ts
		    of NONE => NONE
		     | SOME (st,ts) => SOME (DDef (w, st),ts))))
    fun decl_sentence ts = 
	(case parse_sentence ts
	  of NONE => NONE
	   | SOME (st,ts) => SOME (DSent st, ts))
  in
    choose [decl_def, decl_sentence] ts
  end

  
  and parse_sentence ts = let
    fun sentence_words ts = 
	(case parse_word ts
	  of NONE => NONE
	   | SOME (w,ts) => 
	     (case parse_sentence ts
	       of NONE => SOME (I.SSequence (w,I.SEmpty),ts)
		| SOME (st,ts) => SOME (I.SSequence (w,st),ts)))
    fun sentence_empty ts = SOME (I.SEmpty, ts)
    fun sentence_if ts = 
	(case expect_IF ts
	  of NONE => NONE
	   | SOME ts => 
	     (case parse_sentence ts
	       of NONE => NONE
		| SOME (st1,ts) => 
		  (case expect_THEN ts
		    of NONE => 
		       (case expect_ELSE ts
			 of NONE => NONE
			  | SOME ts => 
			    (case parse_sentence ts
			      of NONE => NONE
			       | SOME (st2,ts) => 
				 (case expect_THEN ts
				   of NONE => NONE
				    | SOME ts => 
				      (case parse_sentence ts
					of NONE => NONE
					 | SOME (st3,ts) => 
					     SOME (I.SIf (st1,st2,st3),ts)))))
		     | SOME ts =>
		       (case parse_sentence ts
			 of NONE => NONE
			  | SOME (st2,ts) => 
			      (SOME (I.SIf (st1,I.SEmpty,st2),ts))))))
  in
    choose [ sentence_if,
	     sentence_words, 
	     sentence_empty
	   ] ts
  end


  and parse_word ts = let
    fun word_int ts = 
	(case expect_INT ts
	  of NONE => NONE
	   | SOME (i,ts) => SOME (I.WInt i,ts))
    fun word_word ts = 
	(case expect_WORD ts
	  of NONE => NONE
	   | SOME (w,ts) => SOME (I.WDefined w,ts))
  in
    choose [ word_int, word_word ] ts
  end

  fun parseDecl ts = 
      (case parse_decl ts
        of SOME (d,[]) => d
         | SOME (_,_)  => parseError "leftover characters past parsed expression"
         | NONE => parseError "cannot parse input")

end
