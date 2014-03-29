(* 
 *   CODE FOR HOMEWORK 5
 *)


structure Evaluator = struct

  structure I = InternalRepresentation



  exception Evaluation of string

  fun evalError msgs = raise Evaluation (String.concatWith " " msgs)


  (*
   *   Environment entries
   *)

  datatype entry = Func of (string list * I.expr * (string * entry) list)
		 | Proc of (string list * I.stmt * (string * entry) list)
		 | Var of I.value ref
		 | Con of I.value


			 
  fun lookup name [] = evalError ["failed lookup for",name]
    | lookup name ((n,v)::env) = 
        if (n = name) then 
	  v
	else lookup name env 


  fun zip [] [] = []
    | zip (x::xs) (y::ys) = (x,y)::(zip xs ys)
    | zip _ _ = evalError ["parameters and arguments don't match"]





  (*
   *   Evaluation functions
   * 
   *)


  fun eval _ (I.EVal v) = v

    | eval env (I.ELet (name,e1,e2)) = eval ((name,Con (eval env e1))::env) e2

    | eval env (I.EIf (e1,e2,e3)) = 
         (case (eval env e1)
	   of I.VBool true => eval env e2
	    | I.VBool false => eval env e3
	    | _ => evalError ["non-Boolean condition"])

    | eval env (I.EIdent n) = 
        (case lookup n env
	  of Var r => !r
	   | Con v => v
	   | _ => evalError ["identifier", n, "not bound to a variable or constant"])

    | eval env (I.ECall (n,es)) = let
	val vs = map (fn e => Con (eval env e)) es
        val f = lookup n env
      in
        case f
	 of Func (ps,body,env) => 
	      eval ((zip ps vs)@[(n,f)]@env) body
	  | _ => evalError ["called identifier not a function"]
      end

    | eval env (I.EPrimCall (f,es)) = f (map (eval env) es)
      




  and exec env (I.SIf (e,s1,s2)) = 
         (case eval env e 
	   of I.VBool true => exec env s1
	    | I.VBool false => exec env s2
	    | _ => evalError ["condition not a Boolean value"])

    | exec env (I.SWhile (e,s)) = 
         (case eval env e
	   of I.VBool true => 
	        (exec env s; exec env (I.SWhile (e,s)))
	    | I.VBool false => ()
	    | _ => evalError ["condition not a Boolean value"])

    | exec env (I.SCall (n,es)) = let
	val vs = map (fn e => Var (ref (eval env e))) es
        val p = lookup n env
      in
	case p
	 of Proc (ps,body,env) => 
	      exec ((zip ps vs)@[(n,p)]@env) body
	  | _ => evalError ["called identifier not a procedure"]
      end

    | exec env (I.SPrint es) = 
      (print (String.concatWith " " 
	      (map (fn e => I.stringOfValue (eval env e)) es));
       print "\n")

    | exec env (I.SBlock []) = ()
    | exec env (I.SBlock (s::ss)) = 
        (exec env s; exec env (I.SBlock ss))

    | exec env (I.SUpdate (n,e)) = let
	val v = eval env e
      in
	case lookup n env
	 of Var r => (r := v)
	  | _ => evalError ["identifier", n, "not a variable"]
      end

    | exec env (I.SPrimCall (f,es)) = f (map (eval env) es)

    | exec env (I.SVar (n,e,s)) = evalError ["exec/SVar not implemented"]


        


  (* 
   *   Initial environment and primitive operations
   *)

  fun primPlus [I.VInt a, I.VInt b] = I.VInt (a+b)
    | primPlus _ = evalError ["type error in primPlus"]

  fun primEq [I.VInt a, I.VInt b] = I.VBool (a=b)
    | primEq [I.VBool a, I.VBool b] = I.VBool (a=b)
    | primEq [I.VList [], I.VList []] = I.VBool true
    | primEq [I.VList (r::rs), I.VList (s::ss)] = 
        (case primEq [!r,!s]
	  of I.VBool true => primEq [I.VList rs, I.VList ss]
           | _ => I.VBool false)
    | primEq [_,_] = I.VBool false
    | primEq _ = evalError ["type error in primEq"]

  fun primCons [a, I.VList (s)] = I.VList((ref a)::s)
    | primCons _ = evalError ["primCons failed"]
  fun primHd [I.VList (s::ss)] = !s
      | primHd _ = evalError ["primHd failed"]
  fun primTl [I.VList (s::ss)] = I.VList(ss)
      | primTl _ = evalError ["primTl failed"]
  fun primUpdateHd [I.VList (s::ss), a] = s := a
      | primUpdateHd _ = evalError ["primUpdateHd failed"]

  val initialEnv = 
      [("+", Func (["a","b"], I.EPrimCall (primPlus,
					   [I.EIdent "a",
					    I.EIdent "b"]),
		    [])),
       ("=", Func (["a","b"], I.EPrimCall (primEq,
					   [I.EIdent "a",
					    I.EIdent "b"]),
		    [])),
       ("nil", Con (I.VList [])),
       ("cons", Func (["a","b"], I.EPrimCall (primCons, 
             [I.EIdent "a", 
             I.EIdent "b"]),
        [])),
        ("hd", Func (["a"], I.EPrimCall (primHd, 
             [I.EIdent "a"]),
        [])),
        ("tl", Func (["a"], I.EPrimCall (primTl, 
             [I.EIdent "a"]),
        [])),
        ("updateHd", Proc (["a","b"], I.SPrimCall (primUpdateHd, 
             [I.EIdent "a", I.EIdent "b"]),
        []))
      ]



  fun info env = let
    fun entryType (Con _) = "(constant)"
      | entryType (Var _) = "(variable)"
      | entryType (Func _) = "(function)"
      | entryType (Proc _) = "(procedure)"
  in
    (app (fn (n,e) => (print (" "^n^"  "^(entryType e)^"\n"))) env)
  end

  fun addFunction env n ps body = (n,Func (ps,body,env))::env

  fun addProcedure env n ps body = (n,Proc (ps,body,env))::env

  fun addVariable env n e = let
    val v = eval env e
    val r = ref v
  in
    (n,Var r)::env
  end

  fun addConstant env n e = let
    val v = eval env e
  in
    (n,Con v)::env
  end
				 
end
