
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
	| subst (ECall (n,f)) id e = ECall (n,)