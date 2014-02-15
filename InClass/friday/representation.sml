
structure InternalRepresentation = struct

  datatype value = VInt of int
  	         | VBool of bool
                 | VList of value list
			    
  datatype expr = EVal of value
		| EAdd of expr * expr
		| EMul of expr * expr
		| EEq of expr * expr
		| EIf of expr * expr * expr
		| ELet of string * expr * expr
		| EIdent of string
		| ECall of string * expr list

		| ECons of expr * expr
		| EHead of expr
		| ETail of expr
			   
  datatype function = FDef of string list * expr 
			      

			      
  fun stringOfValue (VInt i) = Int.toString i
    | stringOfValue (VBool true) = "true"
    | stringOfValue (VBool false) = "false"
    | stringOfValue (VList l) = 
        "("^(String.concatWith " " (map stringOfValue l))^")"


  fun printValue v = (print (stringOfValue v);
		      print "\n")
			

  fun stringOfExpr e = let
    fun $ ss = String.concat ss
    fun $+ ss = String.concatWith "," ss
    fun strCon n f xs = $ [n," (", $+ (map f xs), ")"]
    fun strV (VInt i) = $ ["VInt ",Int.toString i]
      | strV (VBool true) = "VBool true"
      | strV (VBool false) = "VBool false"
      | strV (VList l) = $ ["VList [", $+ (map strV l), "]"]
    fun strS s = "\""^s^"\""
    fun strE (EVal v) = strCon "EVal" strV [v]
      | strE (EAdd (e1,e2)) = strCon "EAdd" strE [e1,e2]
      | strE (EMul (e1,e2)) = strCon "EMul" strE [e1,e2]
      | strE (EEq (e1,e2)) = strCon "EEq" strE [e1,e2]
      | strE (EIf (e1,e2,e3)) = strCon "EIf" strE [e1,e2,e3]
      | strE (ELet (n,e1,e2)) = $ ["ELet (",strS n,",",strE e1,",",strE e2,")"]
      | strE (EIdent n) = $ ["EIdent ", strS n]
      | strE (ECall (n,es)) = $ ["ECall (", strS n,", [", $+ (map strE es),"])"]
      | strE (ECons (e1,e2)) = strCon "ECons" strE [e1,e2]
      | strE (EHead e) = strCon "EHead" strE [e]
      | strE (ETail e) = strCon "ETail" strE [e]
  in
    strE e
  end
		       
end
