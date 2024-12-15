type 'a lazy_tree = 
  | LEmpty  
  | LNode of 'a * (unit -> 'a lazy_tree) * (unit -> 'a lazy_tree) * (unit -> 'a lazy_tree)
;;

type 'a llist = 
| LNil  
| LCons of 'a * (unit -> 'a llist)  (*elemnt i funkcja generujaca reszte listy*)
;;

let rec traverse order tree =
  let rec process_tree t =
    match t with
    | LEmpty -> LNil
    | LNode (value, left, middle, right) ->
        let children = order t in  
        LCons (value, fun () -> process_children children)  (*dodaje wartosc do listy*)
  and process_children = function
    | [] -> LNil  (*brak dzieci*)
    | child :: rest ->
        match child () with
        | LEmpty -> process_children rest  (*puste dziecko -> idzie do reszty *)
        | node -> append_lists (process_tree node) (fun () -> process_children rest)
  and append_lists l1 l2 =
    match l1 with
    | LNil -> l2 ()
    | LCons (x, xs) -> LCons (x, fun () -> append_lists (xs ()) l2)
  in
  process_tree tree
;;


let tree =
  LNode (
    1,
    (fun () -> LNode (
        2,
        (fun () -> LNode (5, (fun () -> LEmpty), (fun () -> LEmpty), (fun () -> LEmpty))),
        (fun () -> LNode (6, (fun () -> LEmpty), (fun () -> LEmpty), (fun () -> LEmpty))),
        (fun () -> LEmpty)
      )),
    (fun () -> LNode (3, (fun () -> LEmpty), (fun () -> LEmpty), (fun () -> LEmpty))),
    (fun () -> LNode (4, (fun () -> LEmpty), (fun () -> LEmpty), (fun () -> LEmpty)))
  )
;;

let tree_empty = LEmpty;;

(*
        1
     /  |  \
    2   3   4
   / \
  5   6
lewa: 125634, prawa: 143265
*)


let left_first = function
  | LEmpty -> []
  | LNode (_, left, middle, right) -> [left; middle; right]
;;

let right_first = function
  | LEmpty -> []
  | LNode (_, left, middle, right) -> [right; middle; left]
;;

let middle_first = function
  | LEmpty -> []
  | LNode (_, left, middle, right) -> [middle; left; right]
;;

let rec to_list = function
  | LNil -> []
  | LCons (x, xs) -> x :: to_list (xs ()) 
;;

let result_list_lazy1 = traverse left_first tree;;
let result_list1 = to_list result_list_lazy1;;

let result_list_lazy2 = traverse right_first tree;;
let result_list2 = to_list result_list_lazy2;;

let result_list_lazy_empty = traverse left_first tree_empty;;
let result_list_empty = to_list result_list_lazy_empty;;