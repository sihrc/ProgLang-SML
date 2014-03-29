
structure InternalRepresentation = struct


  datatype value = VInt of int
		 | VList of value list 

  and sentence = SEmpty
	       | SSequence of word * sentence
	       | SIf of sentence * sentence * sentence

  and word = WInt of int
           | WPrim of (value list -> value list)
           | WDefined of string

  fun stringOfValue (VInt i) = Int.toString i
    | stringOfValue (VList vs) = 
        "["^(String.concatWith "," (map stringOfValue vs))^"]"

  fun stringOfStack [] _ = ""
    | stringOfStack _ 0 = "..."
    | stringOfStack (v::vs) d = String.concat [stringOfValue v, " ", 
					       stringOfStack vs (d-1)]
		       
end
