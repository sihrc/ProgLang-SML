(*
 * PLDI (Spring 2014)
 *
 * Code for HOMEWORK 1
 *
 * Chris Lee
 * 1/27/2014
 *)



fun scaleVec a [] = []
  | scaleVec a (x::xs) = (a*x)::(scaleVec a xs)

fun addVec [] [] = []
  | addVec (x::xs) (y::ys) = (x+y)::(addVec xs ys)
  | addVec _ _ = []

fun inner [] [] = 0
  | inner (x::xs) (y::ys) = (x*y) + (inner xs ys)
  | inner _ _ = 0



(* Question 1 *)


(*Euclid's algorithm @ codeproject.com*)
fun gcd a 0 = a
  |gcd a b = gcd b (a mod b)

fun lcm a b = (a * b) div (gcd a b)

fun exp a n = if n <= 0 then 1 else a * (exp a (n-1))

fun tetra a n = if n <= 0 then 1 else exp a (tetra a (n-1))



(* Question 2 *)

fun sum xs = List.foldr (op +) 0 xs

fun prod xs = List.foldr (op * ) 1 xs

fun every_other [] = []
  |every_other (hd::tl) = if length tl < 2 then [hd] else [hd] @ every_other (List.tl tl)

fun flatten xss = List.foldr (op @) [] xss

fun heads xss = List.foldr (fn (x,y) => (List.hd x)::y) [] (List.filter (fn z => length(z) > 0) xss)

fun tails xss = List.foldr (fn (x,y) => (List.tl x)::y) [] (List.filter (fn z => length(z) > 0) xss)

fun scaleMat a m = List.foldr (fn (x,y) => (scaleVec a x)::y) [] m

fun addMat [] [] = [] 
  |addMat [] _ = raise Fail "cannot add empty matrices"
  |addMat _ [] = raise Fail "cannot add empty matrices"
  |addMat (hd::tl) (hd'::tl') = if (length tl = length tl') then (addVec hd hd')::(addMat tl tl') else raise Fail "cannot add matrices of different dimensions"

fun transpose ([]::_) = []
  |transpose m = (heads m)::transpose (tails m)

fun mulVecMat _ ([]::tl) = []
  |mulVecMat [] _ = []
  |mulVecMat v m = (inner v (heads m)) :: (mulVecMat v (tails m))

fun mulMat m1 m2 = List.foldr (fn (x',y') => (mulVecMat x' m2)::y') [] m1

(* QUESTIONS 3 & 4 *)

exception TypeError of string

exception DivisionByZero of string

datatype value = VInt of int
	       | VVec of int list
	       | VMat of int list list
	       | VRat of int * int

datatype expr = EInt of int
	      | EVec of int list
	      | EMat of int list list
	      | EAdd of expr * expr
	      | ESub of expr * expr
	      | EMul of expr * expr
	      | ENeg of expr        
	      | EDiv of expr * expr

fun simplifyRat r = case r of (x,0) => raise Fail "cannot divide by zero" | (0,x) => VInt(0) 
  | (x,y) => let val z = abs(gcd x y) in 
    let val x' = x*y div abs(y) in
    if z = y orelse z = ~y then VInt (x' div z) else VRat(x' div z, (abs y) div z) end end


fun addRat r s = case (r,s) of ((x,y),(x',y')) => simplifyRat (x * y' + x' * y, y' * y)

fun mulRat r s = case (r,s) of ((x,y),(x',y')) => simplifyRat (x * x', y' * y)

fun negRat r = case r of (x,y) => simplifyRat(~1 * x, y)

fun applyAdd (VInt i) (VInt j) = VInt (i+j)
  | applyAdd (VVec v) (VVec w) = VVec (addVec v w)
  | applyAdd (VMat m) (VMat m') = VMat(addMat m m')
  | applyAdd (VRat a) (VRat b) = addRat a b
  | applyAdd (VRat a) (VInt b) = addRat a (b,1)
  | applyAdd (VInt b) (VRat a) = addRat a (b,1)
  | applyAdd _ _ = raise TypeError "applyAdd"

fun applyMul (VInt i) (VInt j) = VInt (i*j)
  | applyMul (VInt i) (VVec v) = VVec (scaleVec i v)
  | applyMul (VVec v) (VInt i) = VVec (scaleVec i v)
  | applyMul (VVec v) (VVec w) = VInt (inner v w)
  | applyMul (VMat m) (VInt a) = VMat (scaleMat a m)
  | applyMul (VInt a) (VMat m) = VMat (scaleMat a m)
  | applyMul (VVec v) (VMat m) = VVec (mulVecMat v m)
  | applyMul (VMat m) (VMat m') = VMat (mulMat m m')
  | applyMul (VRat a) (VRat b) = mulRat a b
  | applyMul (VRat a) (VInt b) = mulRat a (b,1)
  | applyMul (VInt b) (VRat a) = mulRat a (b,1)
  | applyMul _ _ = raise TypeError "applyMul"

fun applyNeg (VInt i) = VInt (~ i)
  | applyNeg (VVec v) = VVec (scaleVec ~1 v)
  | applyNeg (VMat m) = VMat (scaleMat ~1 m)
  | applyNeg (VRat r) = negRat r

fun applySub a b = applyAdd a (applyNeg b)

fun applyDiv (VInt a) (VInt b) = simplifyRat (a,b)
  | applyDiv (VRat a) (VInt b) = mulRat a (1,b)
  | applyDiv (VInt a) (VRat (b,b')) = mulRat (a,1) (b',b)
  | applyDiv (VRat a) (VRat (b,b')) = mulRat a (b',b)
  | applyDiv _ _ = raise TypeError "applyDiv"

fun eval (EInt i) = VInt i
  | eval (EAdd (e,f)) = applyAdd (eval e) (eval f)
  | eval (ESub (e,f)) = applySub (eval e) (eval f)
  | eval (EMul (e,f)) = applyMul (eval e) (eval f)
  | eval (ENeg e) = applyNeg (eval e)
  | eval (EVec v) = VVec v
  | eval (EMat m) = VMat m
  | eval (EDiv (e,f)) = applyDiv (eval e) (eval f)

