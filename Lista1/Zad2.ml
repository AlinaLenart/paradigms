(*Napisać funkcję hits przyjmującą dwie listy i zliczającą na ilu pozycjach są one równe.*)

let rec hits_helper list1 list2 count =
  if list1 = [] || list2 = [] then
    count  
  else
    let head1 = List.hd list1 in
    let head2 = List.hd list2 in
    let tail1 = List.tl list1 in
    let tail2 = List.tl list2 in
    if head1 = head2 then
      hits_helper tail1 tail2 (count + 1)  
    else
      hits_helper tail1 tail2 count  
;;

let hits list1 list2 =
  hits_helper list1 list2 0  
;;


(*testy*)

let list1 = [1; 0; 1; 1; 1] in
let list2 = [1; 0; 0; 1; 1] in
let test1 = hits list1 list2 in
Printf.printf "Test 1: %d (ma byc 4)\n" test1;

let list3 = ['a'; 'b'; 'c'; 'd'] in
let list4 = ['a'; 'x'; 'c'; 'd'] in
let test2 = hits list3 list4 in
Printf.printf "Test 2: %d (ma byc 3)\n" test2;

let test4 = hits [] [1; 2; 3] in
Printf.printf "Test 3(jedna pusta): %d \n" test4;

let test6 = hits [] [] in
Printf.printf "Test 4 (puste dwie): %d \n" test6;


