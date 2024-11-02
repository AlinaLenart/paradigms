(*Napisać funkcję indexSwap zwracającą listę z zamienionymi miejscami elementami o
indeksach a i b. Jeśli dany indeks nie mieści się w zakresie listy – zwrócić listę oryginalną.*)

let indexSwap (list, a, b) =
  let rec length (lst, count) =
    match lst with
    | [] -> count
    | _ :: t -> length (t, count + 1)
  in

  let rec find_elements (list, count, tempA, tempB) =
    match list with
      | [] -> (tempA, tempB)
      | h :: t when count = a -> find_elements (t, count + 1, Some h, tempB)
      | h :: t when count = b -> find_elements (t, count + 1, tempA, Some h)
      | h :: t -> find_elements (t, count + 1, tempA, tempB)
  in
  
  let rec replace_elements (list, count, tempA, tempB) =
    match list with
      | [] -> []
      | h :: t when count = a && tempB <> None -> (Option.get tempB) :: replace_elements (t, count + 1, tempA, None) (*option.get zwraca Some v lub None *)
      | h :: t when count = b && tempA <> None -> (Option.get tempA) :: replace_elements (t, count + 1, None, tempB)
      | h :: t -> h :: replace_elements (t, count + 1, tempA, tempB)
  in

  let len = length (list, 0) in
  match (a < 0, b < 0, a >= len, b >= len) with
     | (true, _, _, _) | (_, true, _, _) | (_, _, true, _) | (_, _, _, true) -> list
     | _ -> let (tempA, tempB) = find_elements (list, 0, None, None) in
         replace_elements (list, 0, tempA, tempB)
;;



let indexSwap13 list = indexSwap (list, 1, 3)

let list1 = [1; 2; 3; 4; 5];;
let list2 = [6; 7; 8; 9; 10];;
let listEmpty = [];;
let listOutOfIndex = [0; 1];;

indexSwap13 list1;;
indexSwap13 list2;; 
indexSwap13 listEmpty;; 
indexSwap13 listOutOfIndex;; 