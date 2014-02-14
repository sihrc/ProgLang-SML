(*
 * PLDI (Spring 2014)
 *
 * Code for HOMEWORK 3
 *
 * Chris Lee
 * 2/13/2014
 *)



(*
 *   Glue code to give access to regular expression library 
 *)

CM.make "$/regexp-lib.cm";

structure R = RegExpFn (structure P = AwkSyntax structure E = DfaEngine)


(*
 *    Error functions
 *)

exception Parsing of string
exception Evaluation of string


fun evalError str = raise Evaluation str
fun parseError str = raise Parsing str

fun unimplemented fname = raise Fail ("Function "^fname^" not implemented")



(*************************************************************
 *    Internal representation 
 *)

datatype value = VRat of int * int
               | VBool of bool
         | VMat of value list list

datatype expr = EVal of value
              | EAdd of expr * expr
        | ESub of expr * expr
              | EMul of expr * expr
              | EDiv of expr * expr
        | EEq of expr * expr
              | EIf of expr * expr * expr
              | ELet of string * expr * expr
              | EIdent of string
              | ECall of string * expr list
        | EMatrix of expr list list
        | EEye of expr

datatype function = FDef of string list * expr 



(*
 *   Functions for converting elements of the internal representation
 *   into strings, and printing them
 *)

fun stringOfValue (VRat (i,1)) = Int.toString i
  | stringOfValue (VRat (i,j)) = (Int.toString i)^"/"^(Int.toString j)
  | stringOfValue (VBool true) = "true"
  | stringOfValue (VBool false) = "false"
  | stringOfValue (VMat m) = let
  fun row r = String.concatWith " " (map stringOfValue r)
    in
  "["^(String.concatWith " ; " (map row m))^"]"
    end

fun printValue v = print ((stringOfValue v)^"\n")

fun stringOfExpr e = let
    fun $ ss = String.concat ss
    fun $+ ss = String.concatWith "," ss
    fun strCon n f xs = $ [n," (", $+ (map f xs), ")"]
    fun strM f mat = let
      fun row r = $ ["[", $+ (map f r), "]"]
  in
      $ ["[", $+ (map row mat), "]"]
  end
    fun strV (VRat (i,j)) = $ ["VRat (",Int.toString i,",",Int.toString j,")"]
      | strV (VBool true) = "VBool true"
      | strV (VBool false) = "VBool false"
      | strV (VMat m) = strM strV m
    fun strS s = "\""^s^"\""
    fun strE (EVal v) = strCon "EVal" strV [v]
      | strE (EAdd (e1,e2)) = strCon "EAdd" strE [e1,e2]
      | strE (ESub (e1,e2)) = strCon "ESub" strE [e1,e2]
      | strE (EMul (e1,e2)) = strCon "EMul" strE [e1,e2]
      | strE (EDiv (e1,e2)) = strCon "EDiv" strE [e1,e2]
      | strE (EEye (e)) = strCon "EEye" strE [e]
      | strE (EEq (e1,e2)) = strCon "EEq" strE [e1,e2]
      | strE (EIf (e1,e2,e3)) = strCon "EIf" strE [e1,e2,e3]
      | strE (ELet (n,e1,e2)) = $ ["ELet (",strS n,",",strE e1,",",strE e2,")"]
      | strE (EIdent n) = $ ["EIdent ", strS n]
      | strE (ECall (n,es)) = $ ["ECall (", strS n,", [", $+ (map strE es),"])"]
      | strE (EMatrix ess) = $ ["EMatrix ", strM strE ess]
in
    strE e
end





(*************************************************************
 *   Evaluation of internal representation expressions
 *
 *)


(* 
 *   Helper functions for rational numbers 
 *)

fun simplifyRat (n,d) = let
    fun gcd a 0 = a
      | gcd a b = gcd b (a mod b)
    val g = abs (gcd n d)
    val s = Int.sign n * Int.sign d
    val n' = s * abs (n div g)
in
    case abs(d div g) 
     of 0 => evalError "division by zero in simplifyRat"
      | d' => (n',d')
end

fun addRat (n1,d1) (n2,d2) = simplifyRat (n1 * d2 + n2 * d1, d1 * d2)

fun mulRat (n1,d1) (n2,d2) = simplifyRat (n1 * n2, d1 * d2)



(*
 *   Helper functions for matrix operations
 *)

fun height [] = evalError "matrix with no rows"
  | height m = length m
         
fun width [] = evalError "matrix with no rows"
  | width (xs::xss) = length xs

fun matrix [] = evalError "matrix with no rows"
  | matrix (xs::xss) = let
  val columns = length xs
  fun check_lengths [] = []
    | check_lengths (xs::xss) = 
      if length xs = columns
         then xs::(check_lengths xss)
      else evalError "matrix with uneven rows"
    in
  if columns > 0 
      then VMat (xs::(check_lengths xss))
  else evalError "matrix with no columns"
    end


fun mapMat f [] = []
  | mapMat f (xs::xss) = (map f xs)::(mapMat f xss)


fun identityMat n = 
    if n <= 0
      then evalError "size of identity matrix must be at least 1"
    else if n = 1
      then [[VRat (1,1)]]
    else let 
      fun cons0 xs = (VRat (0,1))::xs
      fun zero _ = VRat (0,1)
  in
      (VRat (1,1)::(List.tabulate (n-1,zero)))
    ::(map cons0 (identityMat (n-1)))
  end



(*
 *   Implementation of primitive operations
 *)


fun applyAdd (VRat r) (VRat s) = VRat (addRat r s)
  | applyAdd (VMat m) (VMat n) = VMat (addMat m n)
  | applyAdd v (VMat m) = VMat (mapMat (applyAdd v) m)
  | applyAdd (VMat m) v = let
  fun addRight v w = applyAdd w v 
    in
  VMat (mapMat (addRight v) m)
    end
  | applyAdd _ _ = evalError "applyAdd"

and addMat xss yss = let
    fun addLists [] [] = []
      | addLists (x::xs) (y::ys) = (applyAdd x y)::(addLists xs ys)
      | addLists _ _ = []
    fun add [] [] = []
      | add (xs::xss) (ys::yss) = (addLists xs ys)::(add xss yss)
      | add _ _ = []
in
    if (height xss = height yss andalso width xss = width yss)
      then add xss yss
    else evalError "adding matrices with incompatible sizes"
end


fun applyMul (VRat r) (VRat s) = VRat (mulRat r s)
  | applyMul (VMat m) (VMat n) = VMat (mulMat m n)
  | applyMul v (VMat m) = VMat (mapMat (applyMul v) m)
  | applyMul (VMat m) v = let
  fun mulRight v w = applyMul w v 
    in
  VMat (mapMat (mulRight v) m)
    end
  | applyMul _ _ = evalError "applyMul"

and mulMat xss yss = let
    fun inner [] [] = VRat (0,1)
      | inner (x::xs) (y::ys) = applyAdd (applyMul x y) (inner xs ys)
      | inner _ _ = VRat (0,1)
    fun transpose [] = evalError "transposing matrix with no rows"
      | transpose ([]::xss) = []
      | transpose xss = (map hd xss)::(transpose (map tl xss))
    fun multiply [] zss = []
      | multiply (xs::xss) zss = (map (inner xs) zss)::(multiply xss zss)
in
    if width xss = height yss
      then multiply xss (transpose yss)
    else evalError "multiplying matrices with incompatible sizes"
end


fun applySub (VRat r) (VRat (n,d)) = VRat (addRat r (~n, d))
  | applySub (VMat m) (VMat n) = applyAdd (VMat m) (VMat (negateMat n))
  | applySub (VMat m) (VRat (n,d)) = applyAdd (VMat m) (VRat (~n,d))
  | applySub (VRat r) (VMat m) = applyAdd (VRat r) (VMat (negateMat m))
  | applySub _ _ = evalError "applySub"

and negateMat m = let
    fun neg v = applySub (VRat (0,1)) v
in
    mapMat neg m
end


fun applyDiv (VRat r) (VRat (n,d)) = VRat (mulRat r (d,n))
  | applyDiv (VMat m) (VRat (n,d)) = applyMul (VMat m) (VRat (d,n))
  | applyDiv (VRat r) (VMat m) = applyMul (VRat r) (VMat (recipMat m))
  | applyDiv _ _ = evalError "applyDiv"

and recipMat m = let
    fun recip v = applyDiv (VRat (1,1)) v
in
    mapMat recip m
end


fun applyEq (VRat r) (VRat s) = VBool (r=s)
  | applyEq (VBool b) (VBool c) = VBool (b=c)
  | applyEq _ _ = evalError "applyEq"


fun applyEye (VRat(a,b)) = if a mod b = 0 then VMat(identityMat (a div b)) else evalError "ApplyEye requires integer"
  |applyEye _ = evalError "Please give a VRAT"



(*
 *   Substitution function 
 *)

fun subst (EVal v) id e = EVal v
  | subst (EAdd (f,g)) id e = EAdd (subst f id e, subst g id e)
  | subst (EMul (f,g)) id e = EMul (subst f id e, subst g id e)
  | subst (ESub (f,g)) id e = ESub (subst f id e, subst g id e)
  | subst (EDiv (f,g)) id e = EDiv (subst f id e, subst g id e)
  | subst (EEq (f,g)) id e = EEq (subst f id e, subst g id e)
  | subst (EIf (f,g,h)) id e = EIf (subst f id e, 
                                    subst g id e,
                                    subst h id e)
  | subst (ELet (id',f,g)) id e = 
      if id = id'
      then ELet (id',subst f id e, g)
      else ELet (id',subst f id e, subst g id e)
  | subst (EIdent id') id e = if id = id'
                                then e
                              else EIdent id'
  | subst (ECall (n,fs)) id e = let
  fun sub f = subst f id e 
    in 
  ECall (n, map sub fs)
    end
  | subst (EMatrix m) id e = let
  fun sub f = subst f id e
    in
  EMatrix (mapMat sub m)
    end
  | subst (EEye f) id e = EEye (subst f id e)



(*
 *   Evaluation function
 *)

fun lookup (name:string) [] = evalError "lookup"
  | lookup name ((n,f)::fenv) = 
      if (n = name)
        then f
      else lookup name fenv 

fun eval _ (EVal v) = v
  | eval fenv (EAdd (e,f)) = applyAdd (eval fenv e) (eval fenv f)
  | eval fenv (EMul (e,f)) = applyMul (eval fenv e) (eval fenv f)
  | eval fenv (ESub (e,f)) = applySub (eval fenv e) (eval fenv f)
  | eval fenv (EDiv (e,f)) = applyDiv (eval fenv e) (eval fenv f)
  | eval fenv (EEq (e,f)) = applyEq (eval fenv e) (eval fenv f)
  | eval fenv (EIf (e,f,g)) = evalIf fenv (eval fenv e) f g
  | eval fenv (ELet (name,e,f)) = evalLet fenv name (eval fenv e) f
  | eval _ (EIdent _) = evalError "eval/EId"
  | eval fenv (ECall (name,es)) = 
                evalCall fenv (lookup name fenv) (map (eval fenv) es)
  | eval fenv (EMatrix m) = matrix (mapMat (eval fenv) m)
  | eval fenv (EEye (e)) = applyEye (eval fenv e)

and evalCall fenv (FDef (params,body)) vs = let
    fun substParams e [] [] = e
      | substParams e (id::ids) (v::vs) = 
           substParams (subst e id (EVal v)) ids vs
      | substParams _ _ _ = evalError "evalCall"
in
    eval fenv (substParams body params vs)
end

and evalIf fenv (VBool true) f g = eval fenv f
  | evalIf fenv (VBool false) f g = eval fenv g
  | evalIf _ _ _ _ = evalError "evalIf"

and evalLet fenv id v body = eval fenv (subst body id (EVal v))





(*************************************************************
 *   The lexer
 *
 *   Modified to deal with keywords correctly
 *)


(*
 *   Functions to match regular expressions
 *   (wrappers around the regexp library)
 *
 *   matchRE' matches a compiled regular expression
 *   matchRE matches a string representation of a regular expression
 *)

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

fun matchRE re cs = matchRE' (R.compileString re) cs




(* 
 *   The tokens -- including a way to convert them to strings
 *)

datatype token = T_LET 
         | T_SYM of string 
         | T_INT of int 
         | T_TRUE 
         | T_FALSE
         | T_PLUS
         | T_TIMES
         | T_MINUS
         | T_SLASH
         | T_EQUAL
         | T_IF 
         | T_LPAREN 
         | T_RPAREN
         | T_COMMA
         | T_EYE
         | T_SEMICOLON
         | T_LBRACKET
         | T_RBRACKET
         | T_DEF

fun stringOfToken T_LET = "T_LET"
  | stringOfToken (T_SYM s) = "T_SYM["^s^"]"
  | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
  | stringOfToken T_TRUE = "T_TRUE"
  | stringOfToken T_FALSE = "T_FALSE"
  | stringOfToken T_PLUS = "T_PLUS"
  | stringOfToken T_TIMES = "T_TIMES"
  | stringOfToken T_MINUS = "T_MINUS"
  | stringOfToken T_SLASH = "T_SLASH"
  | stringOfToken T_EQUAL = "T_EQUAL"
  | stringOfToken T_IF  = "T_IF"
  | stringOfToken T_COMMA = "T_COMMA"
  | stringOfToken T_LPAREN = "T_LPAREN"
  | stringOfToken T_RPAREN = "T_RPAREN"
  | stringOfToken T_EYE = "T_EYE"
  | stringOfToken T_DEF = "T_DEF"
  | stringOfToken T_SEMICOLON = "T_SEMICOLON"
  | stringOfToken T_LBRACKET = "T_LBRACKET"
  | stringOfToken T_RBRACKET = "T_RBRACKET"


fun whitespace _ = NONE

fun produceSymbol "let" = SOME (T_LET)
  | produceSymbol "true" = SOME (T_TRUE)
  | produceSymbol "false" = SOME (T_FALSE)
  | produceSymbol "if" = SOME (T_IF)
  | produceSymbol "eye" = SOME (T_EYE)
  | produceSymbol "def" = SOME (T_DEF)
  | produceSymbol text = SOME (T_SYM text)

fun produceInt text = SOME (T_INT (valOf (Int.fromString text)))

fun producePlus _ = SOME (T_PLUS)
fun produceTimes _ = SOME (T_TIMES)
fun produceMinus _ = SOME (T_MINUS)
fun produceEqual _ = SOME (T_EQUAL)
fun produceLParen _ = SOME (T_LPAREN)
fun produceRParen _ = SOME (T_RPAREN)
fun produceSlash _ = SOME (T_SLASH)
fun produceComma _ = SOME (T_COMMA)
fun produceSemiColon _ = SOME (T_SEMICOLON)
fun produceLBracket _ = SOME (T_LBRACKET)
fun produceRBracket _ = SOME (T_RBRACKET)

val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
in
    map convert [("( |\\n|\\t)+",         whitespace),
     ("\\+" ,                 producePlus),
     ("\\*",                  produceTimes),
     ("-",                    produceMinus),
     ("=",                    produceEqual),
     ("[a-zA-Z][a-zA-Z0-9]*", produceSymbol),
     ("~?[0-9]+",             produceInt),
     ("\\(",                  produceLParen),
     ("\\)",                  produceRParen),
     (",",                    produceComma),
     (";",                   produceSemiColon),
     ("\\]",                 produceRBracket),
     ("\\[",                 produceLBracket),
     ("/",                    produceSlash)]
end



(*
 *   Lexing functions
 *)

fun getToken cs = let
    fun loop [] = parseError ("cannot tokenize "^(implode cs))
      | loop ((re,f)::xs) = (case matchRE' re cs of
         NONE => loop xs
             | SOME (m,cs') => (f m,cs'))
in
    loop tokens
end


fun lex []  = []
  | lex cs = let
  val (token,cs') = getToken cs
    in
        case token of 
          NONE => lex cs'
  | SOME t => t::(lex cs')
    end


fun lexString str = lex (explode str)







(************************************************************* 
 *   A SIMPLE PARSER FOR AN ML-LIKE SYNTAX (MULTI-ARGUMENT FUNCTIONS)
 * 
 *   Grammar:
 * 
 *   expr ::= T_IF expr T_THEN expr T_ELSE expr      [expr_IF]
 *            T_LET T_SYM T_EQUAL expr T_IN expr     [expr_LET]
 *            eterm T_EQUAL eterm                    [expr_EQ]
 *            eterm                                  [expr_ETERM]
 *            
 *   expr_list :: = expr T_COMMA expr_list           [expr_list_COMMA]
 *                  expr                             [expr_list_EXPR]
 *                  
 *   eterm ::= term T_PLUS eterm                     [eterm_PLUS]
 *             term T_MINUS eterm                    [eterm_MINUS]
 *             term                                  [eterm_TERM]
 *
 *   term ::= factor T_TIMES term                    [term_TIMES]
 *            factor T_SLASH term                    [term_SLASH]
 *            factor                                 [term_FACTOR]
 *
 *   factor ::= T_INT                                [term_INT]
 *              T_TRUE                               [term_TRUE]
 *              T_FALSE                              [term_FALSE]
 *              T_EYE T_LPAREN expr T_RPAREN         [term_EYE]
 *              T_SYM T_LPAREN expr_list T_RPAREN    [term_CALL]
 *              T_SYM                                [term_SYM]
 *              T_LPAREN expr TRPAREN                [term_PARENS]
 *
 *   expr_rows ::= expr_row T_SEMICOLON expr_rows
 *                 expr_row
 *
 *   expr_row ::= expr expr_row
 *                expr
 * 
 *  (The names on the right are used to refer to the rules
 *   when naming helper parsing functions; they're just indicative)
 *)


fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
  | expect_INT _ = NONE

fun expect_TRUE (T_TRUE::ts) = SOME ts
  | expect_TRUE _ = NONE

fun expect_FALSE (T_FALSE::ts) = SOME ts
  | expect_FALSE _ = NONE

fun expect_SYM ((T_SYM s)::ts) = SOME (s,ts)
  | expect_SYM _ = NONE

fun expect_EYE (T_EYE::ts) = SOME ts
  | expect_EYE _ = NONE

fun expect_IF (T_IF::ts) = SOME ts
  | expect_IF _ = NONE

fun expect_THEN (T_THEN::ts) = SOME ts
  | expect_THEN _ = NONE

fun expect_ELSE (T_ELSE::ts) = SOME ts
  | expect_ELSE _ = NONE

fun expect_LET (T_LET::ts) = SOME ts
  | expect_LET _ = NONE

fun expect_EQUAL (T_EQUAL::ts) = SOME ts
  | expect_EQUAL _ = NONE

fun expect_IN (T_IN::ts) = SOME ts
  | expect_IN _ = NONE

fun expect_LPAREN (T_LPAREN::ts) = SOME ts
  | expect_LPAREN _ = NONE

fun expect_RPAREN (T_RPAREN::ts) = SOME ts
  | expect_RPAREN _ = NONE

fun expect_PLUS (T_PLUS::ts) = SOME ts
  | expect_PLUS _ = NONE

fun expect_TIMES (T_TIMES::ts) = SOME ts
  | expect_TIMES _ = NONE

fun expect_MINUS (T_MINUS::ts) = SOME ts
  | expect_MINUS _ = NONE

fun expect_SLASH (T_SLASH::ts) = SOME ts
  | expect_SLASH _ = NONE

fun expect_COMMA (T_COMMA::ts) = SOME ts
  | expect_COMMA _ = NONE

fun expect_LBRACKET (T_LBRACKET::ts) = SOME ts
  | expect_LBRACKET _ = NONE

fun expect_RBRACKET (T_RBRACKET::ts) = SOME ts
  | expect_RBRACKET _ = NONE

fun expect_SEMICOLON (T_SEMICOLON::ts) = SOME ts
  | expect_SEMICOLON _ = NONE

fun parse_expr ts = 
   (case parse_expr_IF ts
      of NONE => 
   (case parse_expr_LET ts
     of NONE => 
   (case parse_expr_EQ ts
     of NONE => parse_expr_ETERM ts
     | s => s)
     | s => s)
     | s => s)

and parse_expr_row ts = 
  (case parse_expr ts of 
    NONE => NONE
    |SOME (e2, ts) =>
    (case parse_expr_row ts of
      NONE => SOME ([e2], ts)
      |SOME (e3, ts2) => SOME(e2::e3, ts2)))

and parse_expr_rows ts =
  (case parse_expr_row ts of
    NONE => NONE
    |SOME (e2, ts) =>
    (case expect_SEMICOLON ts of 
      NONE => SOME ([e2], ts)
      |SOME (ts) => 
      (case parse_expr_rows ts of 
        NONE => SOME([e2],ts)
        |SOME (e3,ts3) => SOME(e2::e3, ts3))))

and parse_expr_IF ts = 
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
        | SOME (e3,ts) => SOME (EIf (e1,e2,e3),ts)))))))

and parse_expr_LET ts = 
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
        | SOME (e2,ts) => SOME (ELet (s,e1,e2),ts)))))))


and parse_expr_EQ ts = 
    (case parse_eterm ts
      of NONE => NONE
       | SOME (e1,ts) => 
   (case expect_EQUAL ts
     of NONE => NONE
      | SOME ts => 
        (case parse_eterm ts
    of NONE => NONE
     | SOME (e2,ts) => SOME (EEq (e1,e2),ts))))

and parse_expr_ETERM ts = parse_eterm ts



and parse_expr_list ts = 
    (case parse_expr_list_COMMA ts
      of NONE => parse_expr_list_EXPR ts
       | s => s)

and parse_expr_list_COMMA ts = 
    (case parse_expr ts
      of NONE => NONE
       | SOME (e,ts) => 
   (case expect_COMMA ts
     of NONE => NONE
     | SOME ts => 
       (case parse_expr_list ts
         of NONE => NONE
    | SOME (es,ts) => SOME (e::es,ts))))

and parse_expr_list_EXPR ts = 
    (case parse_expr ts
      of NONE => NONE
       | SOME (e,ts) => SOME ([e],ts))



and parse_eterm ts = 
    (case parse_eterm_PLUS ts
      of NONE => 
   (case parse_eterm_MINUS ts
     of NONE => parse_eterm_TERM ts
      | s => s)
       | s => s)

and parse_eterm_PLUS ts = 
    (case parse_term ts 
      of NONE => NONE
       | SOME (e1,ts) => 
   (case expect_PLUS ts
     of NONE => NONE
      | SOME ts => 
        (case parse_eterm ts
    of NONE => NONE
    | SOME (e2,ts) => SOME (EAdd (e1,e2),ts))))

and parse_eterm_MINUS ts = 
    (case parse_term ts 
      of NONE => NONE
       | SOME (e1,ts) => 
   (case expect_MINUS ts
     of NONE => NONE
      | SOME ts => 
        (case parse_eterm ts
    of NONE => NONE
    | SOME (e2,ts) => SOME (ESub (e1,e2),ts))))

and parse_eterm_TERM ts = parse_term ts



and parse_term ts = 
    (case parse_term_TIMES ts
      of NONE => 
   (case parse_term_SLASH ts
     of NONE => parse_term_FACTOR ts
      | s => s)
      | s => s)

and parse_term_TIMES ts = 
    (case parse_factor ts
      of NONE => NONE
       | SOME (e1,ts) => 
   (case expect_TIMES ts
     of NONE => NONE
      | SOME ts => 
        (case parse_term ts
    of NONE => NONE
    | SOME (e2,ts) => SOME (EMul (e1,e2),ts))))

and parse_term_SLASH ts = 
    (case parse_factor ts
      of NONE => NONE
       | SOME (e1,ts) => 
   (case expect_SLASH ts
     of NONE => NONE
      | SOME ts => 
        (case parse_term ts
    of NONE => NONE
    | SOME (e2,ts) => SOME (EDiv (e1,e2),ts))))

and parse_term_FACTOR ts = parse_factor ts




and parse_factor ts = 
    (case parse_factor_INT ts
      of NONE =>
    (case parse_factor_TRUE ts 
      of NONE => 
    (case parse_factor_FALSE ts 
      of NONE => 
    (case parse_factor_CALL ts
      of NONE => 
    (case parse_factor_EYE ts
      of NONE => 
    (case parse_factor_MATRIX ts
      of NONE =>
    (case parse_factor_SYM ts
      of NONE => parse_factor_PARENS ts
                 | s => s)
                | s => s)
               | s => s)
              | s => s)
             | s => s)
            | s => s)
           | s => s)

and parse_factor_MATRIX ts = 
    (case expect_LBRACKET ts 
      of NONE => NONE
       | SOME ts => 
        (case parse_expr_rows ts
    of NONE => NONE
     | SOME (es,ts) => 
       (case expect_RBRACKET ts
         of NONE => NONE
          | SOME ts => SOME (EMatrix(es),ts))))

and parse_factor_EYE ts = 
    (case expect_EYE ts 
      of NONE => NONE
       | SOME ts => 
       (case expect_LPAREN ts
     of NONE => NONE
      | SOME ts => 
        (case parse_expr ts
    of NONE => NONE
     | SOME (es,ts) => 
       (case expect_RPAREN ts
         of NONE => NONE
          | SOME ts => SOME (EEye (es),ts)))))

and parse_factor_INT ts = 
    (case expect_INT ts 
      of NONE => NONE
       | SOME (i,ts) => SOME (EVal (VRat (i,1)),ts))

and parse_factor_TRUE ts = 
    (case expect_TRUE ts
      of NONE => NONE
       | SOME ts => SOME (EVal (VBool true),ts))

and parse_factor_FALSE ts = 
    (case expect_FALSE ts
      of NONE => NONE
       | SOME ts => SOME (EVal (VBool false),ts))

and parse_factor_CALL ts = 
    (case expect_SYM ts 
      of NONE => NONE
       | SOME (s, ts) => 
   (case expect_LPAREN ts
     of NONE => NONE
      | SOME ts => 
        (case parse_expr_list ts
    of NONE => NONE
     | SOME (es,ts) => 
       (case expect_RPAREN ts
         of NONE => NONE
          | SOME ts => SOME (ECall (s,es),ts)))))

and parse_factor_SYM ts = 
    (case expect_SYM ts
      of NONE => NONE
       | SOME (s,ts) => SOME (EIdent s,ts))

and parse_factor_PARENS ts = 
    (case expect_LPAREN ts
      of NONE => NONE
       | SOME ts =>
   (case parse_expr ts
     of NONE => NONE
      | SOME (e,ts) => 
        (case expect_RPAREN ts
    of NONE => NONE
    | SOME ts => SOME (e,ts))))


fun parse tokens = 
    (case parse_expr tokens
      of SOME (e,[]) => e
       | _ => parseError "Cannot parse expression")



datatype decl = DeclDefinition of string * (string list) * expr
              | DeclExpression of expr


fun parse_wdef tokens = unimplemented "parse_wdef"




(*************************************************************
 *   Shells
 *
 *)

    
fun shell fenv = let
    fun prompt () = (print "pldi-hw3> "; TextIO.inputLine (TextIO.stdIn))
    fun pr l = print ((String.concatWith " " l)^"\n")
    fun read fenv = 
  (case prompt () 
    of NONE => ()
     | SOME ".\n" => ()
     | SOME str => eval_print fenv str)
    and eval_print fenv str = 
  (let val ts = lexString str
       val _ = pr (["Tokens ="] @ (map stringOfToken ts))
       val expr = parse ts
       val _ = pr ["Internal rep = ", stringOfExpr (expr)]
       val v = eval fenv expr
       val _ = pr [stringOfValue v]
   in
       read fenv
   end
   handle Parsing msg => (pr ["Parsing error:", msg]; read fenv)
        | Evaluation msg => (pr ["Evaluation error:", msg]; read fenv))
in
    print "Type . by itself to quit\n";
    read fenv
end


    
fun shell_wdef fenv = unimplemented "shell_wdef"

