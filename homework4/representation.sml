(* 
 *   CODE FOR HOMEWORK 4
 *)

structure InternalRepresentation = struct

  datatype value = VInt of int
             | VBool of bool
     | VClosure of string * expr * (string * value) list
     | VRecClosure of string * string * expr * (string * value) list
     | VList of value list
     | VRecord of (string * value) list
          
  and expr = EVal of value
           | EFun of string * expr
           | EIf of expr * expr * expr
           | ELet of string * expr * expr
           | ELetFun of string * string * expr * expr
           | EIdent of string
           | EApp of expr * expr
           | EPrimCall1 of (value -> value) * expr 
           | EPrimCall2 of (value -> value -> value) * expr * expr
           | ERecord of (string * expr) list
           | EField of expr * string

  fun stringOfExpr e = let
    fun $ ss = String.concat ss
    fun $+ ss = String.concatWith "," ss
    fun strCon n f xs = $ [n," (", $+ (map f xs), ")"]
    fun strS s = "\""^s^"\""
    fun strV (VInt i) = $ ["VInt ",Int.toString i]
      | strV (VBool true) = "VBool true"
      | strV (VBool false) = "VBool false"
      | strV (VClosure (n,e,_)) = $ ["VClosure (", n, ",", strE e, ")"]
      | strV (VRecClosure (f,n,e,_)) = $ ["VRecClosure (", f, ",",n, ",", strE e, ")"]
      | strV (VList vs) = $ ["VList [", $+ (map strV vs), "]"]
      | strV (VRecord fvs) = $ ["VRecord [", $+ (map strFV fvs), "]"]
    and strFV (s,v) = $ ["(", s, ",", strV v, ")"]
    and strF (s,e) = $ ["(", s, ",", strE e, ")"]
    and strE (EVal v) = strCon "EVal" strV [v]
      | strE (EFun (n,e)) = $ ["EFun (", n, ",", strE e, ")"]
      | strE (EIf (e1,e2,e3)) = strCon "EIf" strE [e1,e2,e3]
      | strE (ELet (n,e1,e2)) = $ ["ELet (",strS n,",",strE e1,",",strE e2,")"]
      | strE (ELetFun (n,p,e1,e2)) = $ ["ELetFun (",strS n,",",
          strS p, ",",
          strE e1,",",strE e2,")"]
      | strE (EIdent n) = $ ["EIdent ", strS n]
      | strE (EApp (e1,e2)) = strCon "EApp" strE [e1,e2]
      | strE (EPrimCall1 (f,e)) = strCon "EPrimCall1" strE [e]
      | strE (EPrimCall2 (f,e1,e2)) = strCon "EPrimCall2" strE [e1,e2]
      | strE (ERecord fs) = $ ["ERecord [", $+ (map strF fs), "]"]
      | strE (EField (e,s)) = $ ["EField (", strE e, ",", strS s, ")"]
  in
    strE e
  end

  fun stringOfValue (VInt i) = Int.toString i
    | stringOfValue (VBool true) = "true"
    | stringOfValue (VBool false) = "false"
    | stringOfValue (VClosure (n,e,_)) = 
        String.concat ["<function (", n, ",", stringOfExpr e,")>"]
    | stringOfValue (VRecClosure (f,n,e,_)) = 
        String.concat ["<function ", f, " (", n, ",", stringOfExpr e,")>"]
    | stringOfValue (VList vs) = 
        String.concat ["[", String.concatWith "," (map stringOfValue vs), "]"]
    | stringOfValue (VRecord fvs) = let
  fun str (s,v) = String.concat [s, "=", stringOfValue v]
      in
        String.concat ["{", String.concatWith "," (map str fvs), "}"]
      end

  fun printValue v = (print (stringOfValue v);
          print "\n")
           
end
