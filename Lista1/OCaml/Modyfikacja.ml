(*Napisać funkcję split2 przyjmującą listę liczb rzeczywistych oraz dwie wartości: a i b, która
zwraca parę list elementów listy wejściowej. Pierwsza lista ma zawierać elementy ≤ a,
a druga - ≥ b.*)
let rec split2_helper (list, list_a, list_b, a, b) =
  if list = [] then
    (list_a, list_b)
  else
    let head = List.hd list
    and tail = List.tl list 
    in let new_list_a = 
         if head <= a then 
           head :: list_a 
        else list_a
      and new_list_b = 
        if head >= b then 
          head :: list_b 
        else list_b
      in split2_helper (tail, new_list_a, new_list_b, a, b)
;;

let split2 (list, a, b) = 
  split2_helper (list, [], [], a, b)
;;

let list1 = [1; 2; 3; 4; 5; 6];;
let a1 = 3;;
let b1 = 3;;
split2(list1, a1, b1);;

let list2 = [1; 2; 3; 4];;
let a2 = 3;;
let b2 = 13;;
split2 (list2, a2, b2);;

let list3 = [1; 2; 3; 4; 5; 6];;
let a3 = 6;;
let b3 = 1;;
split2 (list3, a3, b3);;

split2 ([], a3, b3);;