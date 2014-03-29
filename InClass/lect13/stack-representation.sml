structure StackRepresentation = struct

  structure I = InternalRepresentation

  datatype sentence = SEmpty
		    | SSequence of word * sentence
		    | SIf of sentence * sentence * sentence
		    | SWhile of sentence * sentence

  and word = WInt of int
           | WPrim of (string * (I.value list -> I.value list))
           | WDefined of string

  fun stringOfWord (WInt i) = Int.toString i
    | stringOfWord (WPrim (n,_)) = "<prim "^n^">"
    | stringOfWord (WDefined w) = w

  fun stringOfSentence (SEmpty) = ""
    | stringOfSentence (SSequence (w,s)) = String.concat [stringOfWord w,
							  " ",
							  stringOfSentence s]
    | stringOfSentence (SIf (s1,s2,s3)) = String.concat ["IF ",
							 stringOfSentence s1,
							 "ELSE ",
							 stringOfSentence s2,
							 "THEN ",
							 stringOfSentence s3]
    | stringOfSentence (SWhile (s1,s2)) = String.concat ["WHILE ",
							 stringOfSentence s1,
							 "REPEAT ",
							 stringOfSentence s2]


  fun stringOfStack [] _ = ""
    | stringOfStack _ 0 = "..."
    | stringOfStack (v::vs) d = String.concat [I.stringOfValue v, " ", 
					       stringOfStack vs (d-1)]

end
