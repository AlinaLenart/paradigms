(*Napisać funkcję hits przyjmującą dwie listy i zliczającą na ilu pozycjach są one równe.*)

let rec hits_helper (list1, list2, count) =
  if list1 = [] || list2 = [] then
    count  
  else
    let head1 = List.hd list1 in
    let head2 = List.hd list2 in
    let tail1 = List.tl list1 in
    let tail2 = List.tl list2 in
    if head1 = head2 then
      hits_helper(tail1, tail2, (count + 1))  
    else
      hits_helper(tail1, tail2, count)  
;;

let hits (list1, list2) =
  hits_helper(list1, list2, 0)  
;;

(*testy*)

let list1 = [1; 0; 1; 1; 1];;
let list2 = [1; 0; 0; 1; 1];;
hits(list1, list2);;

let list3 = ['a'; 'b'; 'c'; 'd'];;
let list4 = ['a'; 'x'; 'c'; 'd'];;
hits(list3, list4);;

hits([], [1; 2; 3]);;
hits([], []);;


