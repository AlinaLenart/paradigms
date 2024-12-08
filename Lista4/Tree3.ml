type 'a tree3 =
  | Empty
  | Node0 of 'a
  | Node1 of { head: 'a; tail: 'a tree3 }
  | Node2 of { head: 'a; left: 'a tree3; right: 'a tree3 }
  | Node3 of { head: 'a; left: 'a tree3; middle: 'a tree3; right: 'a tree3 }

let rec combineTree3 f tree1 tree2 =
  match tree1, tree2 with
  | Empty, _ | _, Empty -> Empty
  | Node0 v1, Node0 v2 -> Node0 (f v1 v2)
  | Node1 { head = v1; tail = t1 }, Node1 { head = v2; tail = t2 } ->
      Node1 { head = f v1 v2; tail = combineTree3 f t1 t2 }
  | Node2 { head = v1; left = l1; right = r1 }, Node2 { head = v2; left = l2; right = r2 } ->
      Node2 { head = f v1 v2; left = combineTree3 f l1 l2; right = combineTree3 f r1 r2 }
  | Node3 { head = v1; left = l1; middle = m1; right = r1 }, Node3 { head = v2; left = l2; middle = m2; right = r2 } ->
      Node3 { head = f v1 v2; left = combineTree3 f l1 l2; middle = combineTree3 f m1 m2; right = combineTree3 f r1 r2 }

  | Node2 { head = v1; left = l1; right = r1 }, Node3 { head = v2; left = l2; middle = m2; right = r2 }
  | Node3 { head = v2; left = l2; middle = m2; right = r2 }, Node2 { head = v1; left = l1; right = r1 } ->
      Node3 { head = f v1 v2; left = combineTree3 f l1 l2; middle = combineTree3 f Empty m2; right = combineTree3 f r1 r2 }

  | Node1 { head = v1; tail = t1 }, Node2 { head = v2; left = l2; right = r2 }
  | Node2 { head = v2; left = l2; right = r2 }, Node1 { head = v1; tail = t1 } ->
      Node2 { head = f v1 v2; left = combineTree3 f t1 l2; right = combineTree3 f Empty r2 }

  | Node1 { head = v1; tail = t1 }, Node3 { head = v2; left = l2; middle = m2; right = r2 }
  | Node3 { head = v2; left = l2; middle = m2; right = r2 }, Node1 { head = v1; tail = t1 } ->
      Node3 { head = f v1 v2; left = combineTree3 f t1 l2; middle = combineTree3 f Empty m2; right = combineTree3 f Empty r2 }

  | Node0 v1, Node1 { head = v2; tail = _ } -> Node0 (f v1 v2)
  | Node0 v1, Node2 { head = v2; left = _; right = _ } -> Node0 (f v1 v2)
  | Node0 v1, Node3 { head = v2; left = _; middle = _; right = _ } -> Node0 (f v1 v2)

  | Node1 { head = v1; tail = _ }, Node0 v2 -> Node0 (f v1 v2)
  | Node2 { head = v1; left = _; right = _ }, Node0 v2 -> Node0 (f v1 v2)
  | Node3 { head = v1; left = _; middle = _; right = _ }, Node0 v2 -> Node0 (f v1 v2)
;;


let emptyTree : int tree3 = Empty;;
let singleNodeTree : int tree3 = Node0 1;;
let singleNodeWithChild : int tree3 = Node1 { head = 1; tail = Node0 1 };;
let singleNodeWithTwoChildren : int tree3 = Node2 { head = 1; left = Node0 1; right = Node0 1 };;
let singleNodeWithThreeChildren : int tree3 = Node3 { head = 1; left = Node0 1; middle = Node0 1; right = Node0 1 };;

let tree1 : int tree3 = 
  Node3 { head = 1; 
          left = Node1 { head = 1; tail = Node0 1 };
          middle = Node0 1;
          right = Node0 1 };;

let tree2 : int tree3 = 
  Node3 { head = 2; 
          left = Node1 { head = 2; tail = Empty }; 
          middle = Empty; 
          right = Node0 2 };;

let combinedTree1 = combineTree3 (fun x y -> x + y) tree1 tree2;;
let combinedTree2 = combineTree3 (fun x y -> x + y) emptyTree tree2;;
let combinedTree3 = combineTree3 (fun x y -> x + y) singleNodeTree tree2;;
let combinedTree4 = combineTree3 (fun x y -> x + y) singleNodeWithTwoChildren singleNodeWithThreeChildren;;