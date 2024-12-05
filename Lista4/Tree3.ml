type 'a tree3 = 
  | Empty  
  | Node of 'a * 'a tree3 option * 'a tree3 option * 'a tree3 option
;;


let rec combineTree3 f tree1 tree2 = 
  match tree1, tree2 with
  | Empty, Empty -> Empty
  | Node (v1, l1, m1, r1), Node (v2, l2, m2, r2) ->
      let combine_option opt1 opt2 =
        match opt1, opt2 with
        | None, None -> None
        | Some t1, None -> Some Empty 
        | None, Some t2 -> Some Empty
        | Some t1, Some t2 -> Some (combineTree3 f t1 t2)
      in
      Node (f v1 v2,
            combine_option l1 l2,
            combine_option m1 m2,
            combine_option r1 r2)
  | Node (v, l, m, r), Empty
  | Empty, Node (v, l, m, r) ->
      let combine_single opt =
        match opt with
        | None -> Some Empty
        | Some t -> Some (combineTree3 f t Empty)
      in
      Node (v,
            combine_single l,
            combine_single m,
            combine_single r)
;;




let empty_tree : int tree3 = Empty;;
let single_node_tree : int tree3 = Node (10, None, None, None);;


let tree1 : int tree3=
  Node (1, 
  Some (Node (2, 
                Some (Node (8, None, None, None)), 
                None, 
                None)), 
  Some (Node (3, None, None, None)), 
  None)
;;

let tree2 : int tree3=
  Node (10,
        Some (Node (20, None, None, None)),
        None,
        Some (Node (30, None, None, None)))
;;


let sum x y = x + y;;
let combined = combineTree3 sum tree1 tree2;;


(* - : val combined : int tree3 =
  Node (11, 
        Some (Node (22, Some Empty, None, None)),
        Some Empty,
        Some Empty)
*)
(*
      1
    / \ \
   2   3 None
  /
 8
*)

(*
       10
    / \    \
  20  None 30
*)

(*
       11
    / \    \
  22  Empty Empty
  /
Empty
*)





