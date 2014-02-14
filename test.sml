use "1.sml";

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