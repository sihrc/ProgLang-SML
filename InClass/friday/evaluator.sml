
structure Evaluator = struct

  structure I = InternalRepresentation



  exception Evaluation of string

  fun evalError msg = raise Evaluation msg


  (* 
   *   Primitive operations
   *)
			    
  fun applyAdd (I.VInt i) (I.VInt j) = I.VInt (i+j)
    | applyAdd _ _ = evalError "applyAdd"
		     
  fun applyMul (I.VInt i) (I.VInt j) = I.VInt (i*j)
    | applyMul _ _ = evalError "applyMul"
		     
  fun applyEq (I.VInt i) (I.VInt j) = I.VBool (i=j)
    | applyEq (I.VBool b) (I.VBool c) = I.VBool (b=c)
    | applyEq (I.VList l1) (I.VList l2) = I.VBool (equalLists l1 l2)
    | applyEq _ _ = evalError "applyEq"

  and equalLists [] [] = true
    | equalLists (v1::vs1) (v2::vs2) = 
      (case applyEq v1 v2
	of I.VBool true => equalLists vs1 vs2
	 | _ => false)
    | equalLists _ _ = false

  fun applyCons v (I.VList l) = I.VList (v::l)
    | applyCons _ _ = evalError "applyCons"

  fun applyHead (I.VList (v::vs)) = v
    | applyHead _ = evalError "applyHead"

  fun applyTail (I.VList (v::vs)) = I.VList vs
    | applyTail _ = evalError "applyTail"




  (*
   *   Substitution function
   *)

  fun subst (I.EVal value) id e = I.EVal value
    | subst (I.EAdd (e1,e2)) id e = I.EAdd (subst e1 id e, subst e2 id e)
    | subst (I.EMul (e1,e2)) id e = I.EMul (subst e1 id e, subst e2 id e)
    | subst (I.EEq (e1,e2)) id e = I.EEq (subst e1 id e, subst e2 id e)
    | subst (I.EIf (e1,e2,h)) id e = 
        I.EIf (subst e1 id e, subst e2 id e, subst h id e)
    | subst (I.ELet (id',e1,e2)) id e = 
        if id = id' then 
	  I.ELet (id',subst e1 id e, e2)
	else I.ELet (id',subst e1 id e, subst e2 id e)
    | subst (I.EIdent id') id e = 
        if id = id' then e else I.EIdent id'
    | subst (I.ECall (n,es)) id e = let
	fun sub e1 = subst e1 id e
      in
	I.ECall (n, map sub es)
      end

    | subst (I.ECons (e1,e2)) id e = I.ECons (subst e1 id e, subst e2 id e)
    | subst (I.EHead e1) id e = I.EHead (subst e1 id e)
    | subst (I.ETail e1) id e = I.ETail (subst e1 id e)
			       
  fun substValues e [] [] = e
    | substValues e (id::ids) (v::vs) = substValues (subst e id (I.EVal v)) ids vs
    | substValues _ _ _ = evalError "substValues"
				     
    
			 
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,f)::fenv) = 
        if (n = name) then 
	  f
	else lookup name fenv 


  (*
   *   Evaluation functions
   *)
	   
  fun eval _ (I.EVal v) = v
    | eval fenv (I.EAdd (e,f)) = applyAdd (eval fenv e) (eval fenv f)
    | eval fenv (I.EMul (e,f)) = applyMul (eval fenv e) (eval fenv f)
    | eval fenv (I.EEq (e,f)) = applyEq (eval fenv e) (eval fenv f)
    | eval fenv (I.EIf (e,f,g)) = evalIf fenv (eval fenv e) f g
    | eval fenv (I.ELet (name,e,f)) = evalLet fenv name e f
    | eval _ (I.EIdent _) = evalError "eval/EId"
    | eval fenv (I.ECall (name,es)) = 
        evalCall fenv (lookup name fenv) (map (eval fenv) es)
    | eval fenv (I.ECons (e1,e2)) = applyCons (eval fenv e1) (eval fenv e2)
    | eval fenv (I.EHead e1) = applyHead (eval fenv e1)
    | eval fenv (I.ETail e1) = applyTail (eval fenv e1)
      
  and evalCall fenv (I.FDef (params,body)) vs = 
        eval fenv (substValues body params vs)
      
  and evalIf fenv (I.VBool true) f g = eval fenv f
    | evalIf fenv (I.VBool false) f g = eval fenv g
    | evalIf _ _ _ _ = evalError "evalIf"
		       
  and evalLet fenv id exp body = eval fenv (subst body id (I.EVal (eval fenv exp)))
				 
end
