CM.make "homework4.cm";
Control.Print.printDepth := 100; 

structure P = Parser;
structure E = Evaluator;
structure I = InternalRepresentation;


val here = "asdfasdf"
(*val test_multiarg = P.parse (P.lexString "let double x = x + x in double 10")*)

(*val test_multiarg2 = P.parse (P.lexString "let max a b = if (a < b) then b else a in max 10 20");*)

fun run str = E.eval E.initialEnv (P.parse (P.lexString str))
fun runI str = case (E.eval E.initialEnv (P.parse (P.lexString str))) of
	I.VInt x => x
fun runV str = case (E.eval E.initialEnv (P.parse (P.lexString str))) of
	I.VList x => (map (fn (I.VInt i) => i)) x 
fun runB str = case (E.eval E.initialEnv (P.parse (P.lexString str))) of
	I.VBool x => x 

val r_test1 = (runI "1+1") = 2;
val r_test2 = (runI "let double x = x + x in double 10") = 20;
val r_test3 = (runI "let sum3 a b c = a + (b + c) in sum3 10 20 30") = 60;
val a2_test_1 = runV "nil" = []
val a2_test_2 = runV "cons 1 nil" = [1]
val a2_test_3 = runV "cons 2 (cons 1 nil)" = [2, 1]

val a2_test_4 = runV "1::(2::nil)" = [1, 2]

val a2_test_5 = runI "hd (1::(2::nil))" = 1

val a2_test_6 = runV "tl (1::nil)" = []
val a2_test_7 = runV "tl (1::(2::nil))" = [2]
val a2_test_8 = runB "nil = nil" = true
val a2_test_9 = runB "nil = 1" = false
val a2_test_10 = runB "nil = 1::nil" = false
val a2_test_11 = runB "1::nil = 1::nil" = true
val a2_test_12 = runB "1::nil = 1::(2::nil)" = false


val b1_test_1 = P.parse_expr_list (P.lexString "1, 2, 3")
val b1_test_2 = runB"[1, 2, 3, 4] = [1, 2, 3, 4]"
val b1_test_3 = runB"(match nil with [] -> 1 | x::xs -> 2) = 1"
val b1_test_4 = runB "(match (1::nil) with [] -> 1 | x::xs -> 2) = 2"
val b1_test_5 = runB "(match nil with [] -> 0 | x::xs -> x + (hd xs)) = 0"
val b1_test_6 = runB  "(match (1::(2::nil)) with [] -> 0 | x::xs -> x + (hd xs)) = 3"
val b1_test_7 = runB "(let sum xs = match xs with [] -> 0 | x::xs -> x + sum xs in sum [1, 2, 3]) = 6"

val d2_test_1 = runB "interval 1 3 = [1, 2, 3]"
val d2_test_2 = runB "interval (1+2) (4+5) = [3,4,5,6,7,8,9]"

val f2_test_1 = runB "[1 .. 10] = [1,2,3,4,5,6,7,8,9,10]"

(*val t = I.EApp(I.EIdent "hd", I.EVal (I.VList [ I.VInt(1)]));
val j = E.eval E.initialEnv t

val f = P.parse (P.lexString "\\x -> (x + x) + 1");
val eval_ed = E.eval E.initialEnv f
val exracted_EFunc = (case eval_ed of
	(I.VClosure (str, expr, fenv)) => I.EFun(str, expr))
val tx = I.EApp(f, I.EVal ( I.VInt(1)));
val jx = E.eval E.initialEnv tx
val g2_test_1 = P.parse (P.lexString "map (\\x -> x+2) [1,2,3,4]")
val gasdf = run  "map (\\x -> x+2) [1,2,3,4]";
*)
val g2_test_3 = runB "map (add 10) [1,2,3,4,5] = [11, 12, 13, 14, 15]"

val new_test_1 = runB "[ x + 1 | x <- [1,2,3,4]] = [2, 3, 4, 5]"
val new_test_2 = runB "[ add 10 x | x <- [1,2,3,4]] = [11, 12, 13, 14]"
val other_new = runB "filter (\\x -> x = 3) [1, 2, 3, 4] = [3]"
val filter_test1 = runB "filter (\\x -> x < 3) [1,2,3,4] = [1, 2]"
val filter_test2 = runB "filter (\\x -> x = 0) [1,2,0,3,4,0,5,6,0] = [0,0,0]"
val that_cool_filter = runB "[ x + x | x <- [1,2,3,4], x < 3] = [2, 4]";

val a3_test = E.eval E.initialEnv (I.ERecord []);
val a3_Test_2 = Evaluator.eval [] (I.ERecord [("a",I.EVal (I.VInt 10))]);
val a3_test3 = Evaluator.eval Evaluator.initialEnv (I.ERecord [("a",I.EApp (I.EApp (I.EIdent
		"add", I.EVal (I.VInt 10)), I.EVal (I.VInt 20)))]);
val a3_test34 = Evaluator.eval [] (I.ERecord [("a",I.EVal (I.VInt 10)),("b",I.EVal (I.VBool
	true))]);

val a3_test5 = run "{}"
val a3_test6 = run "{a=10}"
val a3_test7 = run "{a=10, b=20}"
val a3_test8 = runB "(#b {a=10,b=20}) = 20"
val a3_test9 = runB "(#a {a=10,b=20}) = 10"
val a3_test19 = runB "(#b (let x = 10 in let y = 20 in {a=x,b=y})) = 20"

val done = "DONE!!!!";

