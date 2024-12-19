type 'a lazy_tree =
  | Empty
  | Node of { value: 'a Lazy.t; left: 'a lazy_tree Lazy.t; middle: 'a lazy_tree Lazy.t; right: 'a lazy_tree Lazy.t }

type 'a llist = 
| LNil  
| LCons of 'a * (unit -> 'a llist)  (*elemnt i funkcja generujaca reszte listy*)


let rec traverse order tree =
  let rec process_tree t =
    match t with
    | Empty -> LNil
    | Node { value; left; middle; right } ->
        let value = Lazy.force value in  (*Lazy.force zeby uzyskac wartosc leniwa*)
        let children = order t in
        LCons (value, fun () -> process_children children)  
  and process_children = function
    | [] -> LNil
    | child :: rest ->
        match Lazy.force child with
        | Empty -> process_children rest
        | node -> append_lists (process_tree node) (fun () -> process_children rest)  
  and append_lists l1 l2 =
    match l1 with
    | LNil -> l2 ()
    | LCons (x, xs) -> LCons (x, fun () -> append_lists (xs ()) l2)
  in
  process_tree tree
;;


let tree =
  Node {
    value = lazy 1;  
    left = lazy (Node {
      value = lazy 2;  
      left = lazy (Node {
        value = lazy 5;
        left = lazy Empty;
        middle = lazy Empty;
        right = lazy Empty;
      });
      middle = lazy (Node {
        value = lazy 6;
        left = lazy Empty;
        middle = lazy Empty;
        right = lazy Empty;
      });
      right = lazy Empty;
    });
    middle = lazy (Node {
      value = lazy 3;
      left = lazy Empty;
      middle = lazy Empty;
      right = lazy Empty;
    });
    right = lazy (Node {
      value = lazy 4;
      left = lazy Empty;
      middle = lazy Empty;
      right = lazy Empty;
    });
  }
;;

let tree_empty =
  Node {
    value = lazy Empty;  
    left = lazy Empty;
    middle = lazy Empty;
    right = lazy Empty;
  }
;;

(*
        1
     /  |  \
    2   3   4
   / \
  5   6
125634, *)


let left_first = function
  | Node { left; middle; right; _ } ->
      [ left; middle; right ]  (*ewaluacja leniwa, zwraca lazy.t*)
  | Empty -> [] 
;;

let right_first = function
  | Node { left; middle; right; _ } ->
      [ right; middle; left ]  
  | Empty -> [] 
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