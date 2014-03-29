(* 
 *   CODE FOR HOMEWORK 5
 *)


structure InternalRepresentation = struct

  datatype value = VInt of int
  	         | VBool of bool
		 | VList of (value ref) list
			    
  and expr = EVal of value
	   | ELet of string * expr * expr
	   | EIf of expr * expr * expr
	   | EIdent of string
	   | ECall of string * expr list
           | EPrimCall of (value list -> value) * expr list

  and stmt = SUpdate of string * expr
	   | SIf of expr * stmt * stmt
	   | SWhile of expr * stmt
	   | SCall of string * expr list
	   | SPrint of expr list
           | SBlock of stmt list
	   | SPrimCall of (value list -> unit) * expr list
	   | SVar of (string * expr * stmt)

  fun $ ss = String.concat ss
  fun $+ ss = String.concatWith "," ss
  fun strCon n f xs = $ [n," (", $+ (map f xs), ")"]
  fun strS s = "\""^s^"\""
	       

  fun strV (VInt i) = $ ["VInt ",Int.toString i]
    | strV (VBool true) = "VBool true"
    | strV (VBool false) = "VBool false"
    | strV (VList rl) = $ ["VList [", $+ (map (fn r => strV (!r)) rl), "]"]
  and strE (EVal v) = strCon "EVal" strV [v]
    | strE (ELet (n,e1,e2)) = $ ["ELet (",strS n,",",strE e1,",",strE e2,")"]
    | strE (EIf (e1,e2,e3)) = $ ["SIf (",strE e1, ",", strE e2,",",strE e3,")"]
    | strE (EIdent n) = $ ["EIdent ", strS n]
    | strE (ECall (n,es)) = $ ["ECall (", strS n, ",[", $+ (map strE es), "])"]
    | strE (EPrimCall (f,es)) = strCon "EPrimCall" strE es
  fun strSt (SUpdate (s,e)) = $ ["SUpdate (",strS s,",",strE e,")"]
    | strSt (SIf (e1,s1,s2)) = $ ["SIf (",strE e1, ",", strSt s1,",",strSt s2,")"]
    | strSt (SWhile (e,s)) = $ ["SWhile (",strE e, ",", strSt s,")"]
    | strSt (SCall (n,es)) = $ ["SCall (",strS n,",[",$+ (map strE es),"])"]
    | strSt (SPrint es) = $ ["SPrint [",$+ (map strE es),"]"]
    | strSt (SBlock ss) = $ ["SBlock [",$+ (map strSt ss),"]"]
    | strSt (SPrimCall (f,es)) = strCon "SPrimCall" strE es
    | strSt (SVar (n,e,s)) = $ ["SVar (",strS n,",",strE e,",",strSt s,")"]
			 
			 
  fun stringOfExpr e = strE e

  fun stringOfStmt s = strSt s

  fun stringOfValue (VInt i) = Int.toString i
    | stringOfValue (VBool true) = "true"
    | stringOfValue (VBool false) = "false"
    | stringOfValue (VList rl) = 
      String.concat ["[", 
		     String.concatWith "," 
				       (map (fn r => stringOfValue (!r)) rl),
		     "]"]

  fun printValue v = (print (stringOfValue v);
		      print "\n")
		       
end
