(* 
 *   CODE FOR HOMEWORK 4
 *)


structure Evaluator = struct

  structure I = InternalRepresentation
  structure P = Parser;


  exception Evaluation of string

  fun evalError msg = raise Evaluation msg


  (* 
   *   Primitive operations
   *)

  fun primPlus (I.VInt a) (I.VInt b) = I.VInt (a+b)
    | primPlus _ _ = evalError "primPlus"

  fun primMinus (I.VInt a) (I.VInt b) = I.VInt (a-b)
    | primMinus _ _ = evalError "primMinus"

  fun primEq (I.VInt a) (I.VInt b) = I.VBool (a=b)
    | primEq (I.VBool a) (I.VBool b) = I.VBool (a=b)
    | primEq (I.VList (a::ax)) (I.VList (b::bx)) = if (case (primEq a b) of (I.VBool x) => x
                                                                            |_ => evalError "primEq error") 

      then 
        primEq (I.VList ax) (I.VList bx)
        else 
        I.VBool false
    | primEq (I.VList []) (I.VList []) = I.VBool (true)
    | primEq _ _ = I.VBool false

  fun primLess (I.VInt a) (I.VInt b) = I.VBool (a<b)
    | primLess _ _ = I.VBool false

  fun primCons x (I.VList y) = I.VList(x::y)
    | primCons _ _ = evalError "primCons"
       
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,v)::env) = 
        if (n = name) then 
    v
  else lookup name env 

  fun primHd (I.VList (x::xs)) = x
    | primHd _ = evalError "primHd"

  fun primTl (I.VList (x::xs)) = I.VList xs
    | primTl _ = evalError "primTl"
  
  fun primInterval (I.VInt a) (I.VInt b) = I.VList (List.tabulate ((b - a + 1), (fn x => I.VInt(x + a))))
    | primInterval _ _ = evalError "primInterval"


  (*
   *   Evaluation functions
   * 
   *)


  fun eval _ (I.EVal v) = v
    | eval env (I.EFun (n,e)) = I.VClosure (n,e,env)
    | eval env (I.EIf (e,f,g)) = evalIf env (eval env e) f g
    | eval env (I.ELet (name,e,f)) = evalLet env name (eval env e) f
    | eval env (I.ELetFun (name,param,e,f)) = evalLetFun env name param e f
    | eval env (I.EIdent n) = lookup n env
    | eval env (I.EApp (e1,e2)) = evalApp env (eval env e1) (eval env e2)
    | eval env (I.EPrimCall1 (f,e1)) = f (eval env e1)
    | eval env (I.EPrimCall2 (f,e1,e2)) = f (eval env e1) (eval env e2)
    | eval env (I.ERecord fs) =  I.VRecord (map (fn (str, expr) => (str, eval env expr)) fs)
    | eval env (I.EField (e, s)) = (lookup s (case (eval env e) of (I.VRecord e) => e
                                                                    | _ => evalError "eval EField error"))
      
  and evalApp _ (I.VClosure (n,body,env)) v = eval ((n,v)::env) body
    | evalApp _ (I.VRecClosure (f,n,body,env)) v = let
    val new_env = [(f,I.VRecClosure (f,n,body,env)),(n,v)]@env
      in 
    eval new_env body
      end
    | evalApp _ _ _ = evalError "cannot apply non-functional value"

  and evalIf env (I.VBool true) f g = eval env f
    | evalIf env (I.VBool false) f g = eval env g
    | evalIf _ _ _ _ = evalError "evalIf"
           
  and evalLet env id v body = eval ((id,v)::env) body

  and evalLetFun env id param expr body = let
      val f = I.VRecClosure (id, param, expr, env)
  in
      eval ((id,f)::env) body
  end

 fun primMap (I.VClosure(str,expr,fenv))  (I.VList (a::ax)) = let
  val vfunc = I.EFun(str, expr)
  val inside = eval fenv (I.EApp( vfunc, I.EVal (a)))
  val outer_list = (primMap (I.VClosure (str, expr, fenv)) (I.VList ax))
  in
    case outer_list of 
      I.VList xs => I.VList(inside::xs)
      |_ => evalError "outer list error"
  end
  | primMap (I.VClosure(str,expr,fenv)) (I.VList []) = (I.VList [])
  | primMap _ _ = evalError "primMap error"
  (* 
   *   Initial environment (already in a form suitable for the environment)
   *)

  val initialEnv = 
      [("add", I.VClosure ("a", 
         I.EFun ("b", 
           I.EPrimCall2 (primPlus,
             I.EIdent "a",
             I.EIdent "b")),
         [])),
       ("sub", I.VClosure ("a", 
         I.EFun ("b", 
           I.EPrimCall2 (primMinus,
             I.EIdent "a",
             I.EIdent "b")),
         [])),
       ("equal", I.VClosure ("a",
        I.EFun ("b",
          I.EPrimCall2 (primEq,
            I.EIdent "a",
            I.EIdent "b")),
        [])),
       ("less", I.VClosure ("a",
          I.EFun ("b",
            I.EPrimCall2 (primLess,
              I.EIdent "a",
              I.EIdent "b")),
          [])),
       ("nil", I.VList []),
       ("cons", I.VClosure ("a", 
         I.EFun ("b", 
           I.EPrimCall2 (primCons,
             I.EIdent "a",
             I.EIdent "b")),
         [])),
       ("hd", I.VClosure ("a", 
           I.EPrimCall1 (primHd, I.EIdent "a"),
         [])),
       ("tl", I.VClosure ("a", 
           I.EPrimCall1 (primTl, I.EIdent "a"),
         [])),
        ("interval", I.VClosure ("a",
          I.EFun ("b",
            I.EPrimCall2 (primInterval,
              I.EIdent "a",
              I.EIdent "b")),
          []))
        
       ]
    val initialEnv = ("map", let
          val parsed = P.parse (P.lexString 
            "let map f lst = if (lst = nil) then nil else (cons (f (hd lst)) (map f (tl lst))) in map"
              )
          val efun = (case parsed of (I.ELetFun (_, _,  func, _)) => func
                                  | _ => evalError "map eval error")
          in
            I.VRecClosure("map", "f", efun, initialEnv)
          end)::initialEnv

    val initialEnv = ("filter", let
      val parsed = P.parse (P.lexString 
        "let filter f lst = if lst = nil then nil else if (f (hd lst))=true then (cons (hd lst) (filter f (tl lst))) else (filter f (tl lst)) in filter"
          )
      val efun = (case parsed of (I.ELetFun (_, _,  func, _)) => func
                              | _ => evalError "filter eval error")
      in
        I.VRecClosure("filter", "f", efun, initialEnv)
      end)::initialEnv
         
end
