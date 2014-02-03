
(* Code from Lecture 04 - Functions *)

(* in-class exercise: multiargument functions *)


datatype value = VInt of int
							 | VVec of int list
							 | VBool of bool

datatype expr = EVal of value
							| EAdd of expr * expr
							| ESub of expr * expr
							| EMul of expr * expr
							| ENeg of expr
							| EEq of expr * expr
							| EAnd of expr * expr
							| EIf of expr * expr * expr
							| ELet of string * expr * expr
							| EIdent of string
							| ECall of string * expr list

datatype function = FDef of string list * expr   (* (params,body) *)

fun scaleVec a [] = []
	| scaleVec a (x::xs) = (a*x)::(scaleVec a xs)

fun addVec [] [] = []
	| addVec (x::xs) (y::ys) = (x+y)::(addVec xs ys)
	| addVec _ _ = []

fun inner [] [] = 0
	| inner (x::xs) (y::ys) = (x*y)+(inner xs ys)
	| inner _ _ = 0

exception TypeError of string
exception EvalError of string

fun applyAdd (VInt i) (VInt j) = VInt (i+j)
	| applyAdd (VVec v) (VVec w) = if length v = length w
																	 then VVec (addVec v w)
																 else raise TypeError "applyAdd"
	| applyAdd _ _ = raise TypeError "applyAdd"

fun applyMul (VInt i) (VInt j) = VInt (i*j)
	| applyMul (VInt i) (VVec v) = VVec (scaleVec i v)
	| applyMul (VVec v) (VInt i) = VVec (scaleVec i v)
	| applyMul (VVec v) (VVec w) = if length v = length w
				then VInt (inner v w)
				else raise TypeError "applyMul"
	| applyMul _ _ = raise TypeError "applyMul"

fun applyNeg (VInt i) = VInt (~ i)
	| applyNeg (VVec v) = VVec (scaleVec ~1 v)
	| applyNeg _ = raise TypeError "applyNeg"


fun equalLists [] [] = true
	| equalLists (x::xs) (y::ys) = (x = y) andalso (equalLists xs ys)
	| equalLists _ _ = false

fun applyEq (VInt i) (VInt j) = VBool (i = j)
	| applyEq (VVec v) (VVec w) = VBool (equalLists v w)
	| applyEq (VBool b) (VBool c) = VBool (b = c)
	| applyEq _ _ = raise TypeError "applyEq"

fun applyAnd (VBool b) (VBool c) = VBool (b andalso c)
	| applyAnd _ _ = raise TypeError "applyAnd"

fun applySub a b = applyAdd a (applyNeg b)

fun subst (EVal value) id e = EVal value
	| subst (EAdd (f,g)) id e = EAdd (subst f id e, subst g id e)
	| subst (ESub (f,g)) id e = ESub (subst f id e, subst g id e)
	| subst (EMul (f,g)) id e = EMul (subst f id e, subst g id e)
	| subst (ENeg f) id e = ENeg (subst f id e)
	| subst (EEq (f,g)) id e = EEq (subst f id e, subst g id e)
	| subst (EAnd (f,g)) id e = EAnd (subst f id e, subst g id e)
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
	| subst (ECall (n,es)) = evalCall fe (lookup n fe) (evalList fe es)

and evalCall fe (FDef (ps, body)) vs = eval fe (substAll body ps vs)

and evalList fe [] = []
	|evalList fe (e::es) = (eval fe e)::(evalList fe es)

fun substAll e [] [] = e
	|substAll e (id::ids) (v::vs) = substAll (subst e id (EVal v )) ids vs 
	|substAll _ _ _ = raise EvalError "substAll"
	
fun lookup (name:string) [] = raise EvalError "lookup"
	| lookup name ((n,f)::fenv) = 
			if (n = name)
				then f
			else lookup name fenv 

fun eval _ (EVal v) = v
	| eval fenv (EAdd (e,f)) = applyAdd (eval fenv e) (eval fenv f)
	| eval fenv (ESub (e,f)) = applySub (eval fenv e) (eval fenv f)
	| eval fenv (EMul (e,f)) = applyMul (eval fenv e) (eval fenv f)
	| eval fenv (ENeg e) = applyNeg (eval fenv e)
	| eval fenv (EEq (e,f)) = applyEq (eval fenv e) (eval fenv f)
	| eval fenv (EAnd (e,f)) = evalAnd fenv (eval fenv e) f
	| eval fenv (EIf (e,f,g)) = evalIf fenv (eval fenv e) f g
	| eval fenv (ELet (name,e,f)) = evalLet fenv name (eval fenv e) f
	| eval _ (EIdent _) = raise EvalError "eval/EId"
	| eval fenv (ECall (name,expr)) = 
								evalCall fenv (lookup name fenv) (eval fenv expr)

and evalCall fenv (FDef (param,body)) v = 
			 eval fenv (subst body param (EVal v))

and evalIf fenv (VBool true) f g = eval fenv f
	| evalIf fenv (VBool false) f g = eval fenv g
	| evalIf _ _ _ _ = raise TypeError "evalIf"

and evalAnd fenv (VBool true) f = eval fenv f
	| evalAnd _ (VBool false) f = VBool false
	| evalAnd _ _ _ = raise TypeError "evalAnd"

and evalLet fenv id v body = eval fenv (subst body id (EVal v))
												

(* Some sample expressions presented in class to evaluate *)

val sample1 = EIf (EAnd (EVal (VBool false), EVal (VBool true)),
									 EAdd (EVal (VInt 1), EVal (VInt 2)),
									 EAdd (EVal (VInt 1), EVal (VInt 3)))

val sample2 = EAdd (EVal (VInt 1),
										EIf (EAnd (EVal (VBool false), EVal (VBool true)),
												 EVal (VInt 2), EVal (VInt 3)))

val sample3 = ELet ("x", EAdd (EVal (VInt 10), EVal (VInt 10)),
												 EMul (EIdent "x", EIdent "x"))

val sample4 = ELet ("x", EVal (VInt 10),
										ELet ("y", EIdent "x",
													ELet ("x", EVal (VInt 30),
																EMul (EIdent "x", EIdent "y"))))

val succ = ("succ", FDef (["n"], EAdd (EIdent "n", EVal (VInt 1))))

val pred = ("pred", FDef (["n"], EAdd (EIdent "n", EVal (VInt ~1))))

val fact = ("fact", 
			FDef (["n"], 
			EIf (EEq (EIdent "n", EVal (VInt 0)),
								 EVal (VInt 1),
					 EMul (EIdent "n", 
					 ECall ("fact", [ECall ("pred", [EIdent "n"])])))))
		 
