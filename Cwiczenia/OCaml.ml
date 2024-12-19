































(*zad 5.1*)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lrepeat k llist =
	let rec repeatElement(element, reps, rest) =
		if reps = 0 then rest
		else repeatElement(element, reps - 1, LCons(element, function () -> rest))
	in match llist with
	| LNil -> LNil
	| LCons(firstElement, tailFunction) -> repeatElement(firstElement, k, (lrepeat k (tailFunction())));;


(*zad 5.2*)
let lfib =
  let rec fibgen a b =
      LCons(a, fun () -> fibgen b (a + b))
  in fibgen 0 1;;


  
(*zad 5.3a*)
let rec lTree n =
	LNode(n, (function () -> lTree (2 * n)), function () -> lTree (2 * n + 1));;


(*zad 5.3b*)

let toLazyList tree =
	let rec helper = function
		[] -> LNil
		| LEmpty :: tail -> helper tail
		| LNode(value, leftSubtree, rightSubtree) :: tail -> LCons(value, lazy helper(tail @ [leftSubtree(); rightSubtree()]))
	in helper [tree];;











(*zad 4.3*)
let breadthBT tree =
  let rec helper = function
    [] -> []
    | Empty :: tail -> helper tail
    | Node(value, leftSubtree, rightSubtree) :: tail ->	 value :: helper (leftSubtree :: rightSubtree :: tail)
    in helper [tree];;

(*zad 4.4a*)
let internalPath tree =
	let rec helper depth = function
		Empty -> 0
		| Node(_, leftSubtree, rightSubtree) -> depth + helper (depth + 1) leftSubtree  + helper (depth + 1) rightSubtree 
	in helper 0 tree;;

(*zad 4.4b*)
let externalPath tree =
	let rec helper depth = function
		Empty -> depth
		| Node(_, leftSubtree, rightSubtree) -> helper (depth + 1) leftSubtree + helper (depth + 1) rightSubtree 
	in helper 0 tree;;

(*zad 4.5*)
let depthSearch (Graph succ) startNode =   
  let rec search visited = function
      [] -> []  
      | h :: t -> if List.mem h visited then search visited t 
                    else h :: search (h :: visited) (succ h @ t)
  in search [] [startNode];; 