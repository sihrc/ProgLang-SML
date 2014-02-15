

structure Sample = struct

  structure I = InternalRepresentation
  structure P = Parser

  fun function name params body = let
    val bodyExpr = P.parse (P.lexString body)
  in
    (name, I.FDef (params, bodyExpr))
  end


  val length = function "length" ["l"]
			"(if (= l nil) 0 (+ 1 (length (tail l))))"

  val sum = function "sum" ["l"]
		     "(if (= l nil) 0 (+ (head l) (sum (tail l))))"

  val square = function "square" ["l"]
	       "(if (= l nil) nil (cons (let ((x (head l))) (* x x)) (square (tail l))))"

(*   [need multiargument functions]
  val add = function "add" ["l1","l2"]
	    "(if (= l1 nil) nil (if (= l2 nil) nil (cons (+ (head l1) (head l2)) (add (tail l1) (tail l2)))))"
*)

end
