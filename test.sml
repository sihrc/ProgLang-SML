use "chrislee_hw3.sml";
Control.Print.printDepth := 100; 
val a = applyEye (VRat (1,1)) = VMat [[VRat (1,1)]];
val a =applyEye (VRat (2,1)) = VMat [[VRat (1,1),VRat (0,1)],[VRat (0,1),VRat (1,1)]];
val a =applyEye (VRat (4,1)) = VMat ([[VRat (1,1),VRat (0,1),VRat (0,1),VRat (0,1)],
[VRat (0,1),VRat (1,1),VRat (0,1),VRat (0,1)],
[VRat (0,1),VRat (0,1),VRat (1,1),VRat (0,1)],
[VRat (0,1),VRat (0,1),VRat (0,1),VRat (1,1)]]);

val a = subst (EEye (EIdent "x")) "x" (EVal (VRat (1,1))) = EEye (EVal (VRat (1,1)));
val a = subst (EEye (EAdd (EIdent "x", EIdent "x"))) "x" (EVal (VRat (2,1))) = EEye (EAdd (EVal (VRat (2,1)),EVal (VRat (2,1))));
val a = subst (EEye (EAdd (EIdent "y", EIdent "x"))) "x" (EVal (VRat (2,1))) = EEye (EAdd (EIdent "y",EVal (VRat (2,1))));

val a =  eval [] (EEye (EVal (VRat (2,1)))) =VMat [[VRat (1,1),VRat (0,1)],[VRat (0,1),VRat (1,1)]];
val a =  eval [] (EEye (EVal (VRat (4,1)))) = VMat [[VRat (1,1),VRat (0,1),VRat (0,1),VRat (0,1)], [VRat (0,1),VRat (1,1),VRat (0,1),VRat (0,1)],[VRat (0,1),VRat (0,1),VRat (1,1),VRat (0,1)], [VRat (0,1),VRat (0,1),VRat (0,1),VRat (1,1)]];
val a =  eval [] (EEye (EAdd (EVal (VRat (3,1)), EVal (VRat (1,1))))) = VMat [[VRat (1,1),VRat (0,1),VRat (0,1),VRat (0,1)], [VRat (0,1),VRat (1,1),VRat (0,1),VRat (0,1)], [VRat (0,1),VRat (0,1),VRat (1,1),VRat (0,1)], [VRat (0,1),VRat (0,1),VRat (0,1),VRat (1,1)]];
val a = eval [] (ELet ("x", EVal (VRat (4,2)), EEye (EMul (EIdent "x", EIdent "x")))) = VMat [[VRat (1,1),VRat (0,1),VRat (0,1),VRat (0,1)], [VRat (0,1),VRat (1,1),VRat (0,1),VRat (0,1)], [VRat (0,1),VRat (0,1),VRat (1,1),VRat (0,1)], [VRat (0,1),VRat (0,1),VRat (0,1),VRat (1,1)]];

val b = lexString "eye" = [T_EYE];
val b = lexString "Popeye" = [T_SYM "Popeye"];
val b = lexString "eyeball" = [T_SYM "eyeball"];
val b = lexString "poke out an eye" = [T_SYM "poke",T_SYM "out",T_SYM "an",T_EYE];
val b = lexString "eye (3)" = [T_EYE,T_LPAREN,T_INT 3,T_RPAREN];


val c = parse_factor (lexString "eye (1)") = SOME (EEye (EVal (VRat (1,1))),[]);
val c = parse_factor (lexString "eye (2)") = SOME (EEye (EVal (VRat (2,1))),[]);

val c = parse (lexString "eye (2)") = EEye (EVal (VRat (2,1)));
val c = parse (lexString "1 + eye (2)") = EAdd (EVal (VRat (1,1)),EEye (EVal (VRat (2,1))));
val c = parse (lexString "eye (1+2) + eye (2)") = EAdd (EEye (EAdd (EVal (VRat (1,1)),EVal (VRat (2,1)))), EEye (EVal (VRat (2,1))));



val d = lexString "[" = [T_LBRACKET];
val d = lexString "]" = [T_RBRACKET];
val d = lexString ";" = [T_SEMICOLON];
val d = lexString "[];" = [T_LBRACKET,T_RBRACKET,T_SEMICOLON];
val d = lexString "[1 2 3]" = [T_LBRACKET,T_INT 1,T_INT 2,T_INT 3,T_RBRACKET];
val d = lexString "[1 2 3; 4 5 6]" = [T_LBRACKET,T_INT 1,T_INT 2,T_INT 3,T_SEMICOLON,T_INT 4,T_INT 5,T_INT 6, T_RBRACKET];



val e = parse_factor (lexString "[ 1 2 ]") = SOME (EMatrix [[EVal (VRat (1,1)),EVal (VRat (2,1))]],[]);
val e = parse_factor (lexString "[ 1 2 ; 3 4]") = SOME (EMatrix [[EVal (VRat (1,1)),EVal (VRat (2,1))],[EVal (VRat (3,1)),EVal (VRat (4,1))]],[]);
val e = parse_factor (lexString "[ 1 (2 + 3)]") = SOME(EMatrix [[EVal (VRat (1,1)),EAdd (EVal (VRat (2,1)),EVal (VRat (3,1)))]],[]);
val e = parse_expr_rows (lexString "1 2 ; 3 4") =SOME([[EVal (VRat (1,1)),EVal (VRat (2,1))],[EVal (VRat (3,1)),EVal (VRat (4,1))]],[])
val e = parse_expr_rows (lexString "1 2") = SOME ([[EVal (VRat (1,1)),EVal (VRat (2,1))]],[])
val e = parse_expr_row (lexString "1 2") = SOME ([EVal (VRat (1,1)),EVal (VRat (2,1))],[])
val e = parse_expr_row (lexString "1") = SOME ([EVal (VRat (1,1))],[]);
val e = parse (lexString "[1 2; 3 4]") =EMatrix([[EVal (VRat (1,1)),EVal (VRat (2,1))],[EVal (VRat (3,1)),EVal (VRat (4,1))]]);
val e = parse (lexString "1 + [1 2; 3 4]") = EAdd (EVal (VRat (1,1)), EMatrix [[EVal (VRat (1,1)),EVal (VRat (2,1))], [EVal (VRat (3,1)),EVal (VRat (4,1))]]);


val f = lexString "def" = [T_DEF];
val f = lexString "define" = [T_SYM "define"];
val f = lexString "fundef" = [T_SYM "fundef"];
val f = lexString "def double (x) = 2 * x" =[T_DEF,T_SYM "double",T_LPAREN,T_SYM "x",T_RPAREN,T_EQUAL,T_INT 2,T_TIMES,T_SYM "x"];


val g = parse_decl (lexString "def double (x) = 2 * x") = SOME (DeclDefinition ("double",["x"],EMul (EVal (VRat (2,1)),EIdent "x")),[]);
val g = parse_decl (lexString "double (10)") = SOME (DeclExpression (ECall ("double",[EVal (VRat (10,1))])),[]);
val g = parse_decl (lexString "def sum (a,b,c) = a + b + c") = SOME (DeclDefinition ("sum",["a","b","c"],EAdd (EIdent "a",EAdd (EIdent "b",EIdent "c"))),[]);
val g = parse_decl (lexString "a + b + c") = SOME (DeclExpression (EAdd (EIdent "a",EAdd (EIdent "b",EIdent "c"))),[]);
val g = parse_decl (lexString "1 + sum (10,20,30)") = SOME (DeclExpression (EAdd (EVal (VRat (1,1)), ECall ("sum", [EVal (VRat (10,1)),EVal (VRat (20,1)),EVal (VRat (30,1))]))), []);


val h = parse_wdef (lexString "def double (x) = 2 * x")  = DeclDefinition ("double",["x"],EMul (EVal (VRat (2,1)),EIdent "x"));
val h = parse_wdef (lexString "double (10)")  = DeclExpression (ECall ("double",[EVal (VRat (10,1))]));
val h = parse_wdef (lexString "def sum (a,b,c) = a + b + c")  = DeclDefinition ("sum",["a","b","c"],EAdd (EIdent "a",EAdd (EIdent "b",EIdent "c")));

shell_wdef [];