structure Compiler = struct

  structure I = InternalRepresentation
  structure S = StackRepresentation
  structure P = Primitives


  exception Compilation of string

  fun compileError msg = raise Compilation msg



  (* 
   *   helper functions for working with sentences
   * 
   *  appendS : sentence -> sentence -> sentence // appends two sentences
   *  appendSs : sentence list -> sentence       // appends list of sentences
   *  wordS : word -> sentence                   // sentence with only a word
   *)
  
  fun appendS (S.SEmpty) s = s
    | appendS (S.SSequence (w,s1)) s2 = S.SSequence (w,appendS s1 s2)
    | appendS (S.SIf (s1,s2,s3)) s4 = S.SIf (s1,s2,appendS s3 s4)
    | appendS (S.SWhile (s1,s2)) s3 = S.SWhile (s1,appendS s2 s3)

  fun appendSs [] = S.SEmpty
    | appendSs (s1::ss) = appendS s1 (appendSs ss)

  fun wordS (w) = S.SSequence (w,S.SEmpty)



  (* TO COMPLETE *)

  fun compileV (I.VInt i) = wordS (S.WInt i)
    | compileV (I.VBool true) = wordS (S.WInt 1)
    | compileV (I.VBool false) = wordS (S.WInt 0)
    | compileV (I.VList l) = appendSs [(wordS (S.WDefined "nil")), compileList l]

  and compileList (l::ls) = appendSs [(wordS (S.WDefined "cons")), (compileV l),(compileList ls)]

  and compileE (I.EVal v) = compileV v
    | compileE (I.EAdd (e1,e2)) = appendSs [(compileE e1),(compileE e2),(wordS (S.WPrim ("Add", P.primAdd)))]
    | compileE (I.ESub (e1,e2)) = appendSs [(compileE e1),(compileE e2),(wordS (S.WPrim ("Sub", P.primSub)))]
    | compileE (I.EMul (e1,e2)) = appendSs [(compileE e1),(compileE e2),(wordS (S.WPrim ("Mul", P.primMul)))]
    | compileE (I.EEq (e1,e2)) = appendSs [(compileE e1),(compileE e2),(wordS (S.WPrim ("Eq", P.primEq)))]
    | compileE (I.ECons (e1,e2)) = appendSs [(compileE e1),(compileE e2),(wordS (S.WPrim ("Cons", P.primCons)))]
    | compileE (I.EHead e1) = appendSs [(compileE e1),(wordS (S.WPrim ("Head", P.primHead)))]
    | compileE (I.ETail e1) = appendSs [(compileE e1),(wordS (S.WPrim ("Tail", P.primTail)))]
    | compileE (I.EIf (e1,e2,e3)) = S.SIf ((compileE e1),(compileE e2),(compileE e2))

    | compileE (I.ELet (name,e1,e2)) = compileError "compileE/ELet"
    | compileE (I.EIdent name) = compileError "compileE/EIdent"

    | compileE (I.ECall (name,es)) = compileError "compileE/ECall"


  fun compileExpr expr = let
      val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])
      val sent = compileE expr
      val _ = print (String.concat ["[  ", S.stringOfSentence sent, " ]\n"])
  in
      sent
  end



  (* TO COMPLETE -- a definition should return a sentence associated with
   *  the name being defined. Calling the name from the stack language
   *  with the arguments on the stack in the right order should execute
   *  the body of the compiled function *)

  fun compileDef name params expr = let
    val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])
    val sent = compileError "cannot compile definitions yet"
    val _ = print (String.concat ["[  ", S.stringOfSentence sent, " ]\n"])
  in
    sent
  end



end
