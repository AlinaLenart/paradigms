(*Napisać funkcję indexSwap zwracającą listę z zamienionymi miejscami elementami o
indeksach a i b. Jeśli dany indeks nie mieści się w zakresie listy – zwrócić listę oryginalną.
Funkcja ma być napisana w taki sposób, by za jej pomocą można było wygenerować wiele
funkcji zamieniającymi elementy między danymi pozycjami np. indexSwap15, która zamienia
elementy o indeksach 1 i 5. Wykorzystać mechanizm dopasowania wzorca, ale bez wyrażenia if.*)

let rec split3Rec list =
    match list with
        x :: y :: z :: tail -> let l1, l2, l3 = split3Rec tail 
        in x :: l1, y :: l2, z :: l3
    | _ -> [], [], []
;;

let split3Tail list = 
    let rec split3Tail_helper accum list = 
      match list, accum with
        x :: y :: z :: tail, (l1, l2, l3) -> split3Tail_helper(x::l1, y::l2, z::l3) tail
      | _, _ -> accum in split3Tail_helper ([], [], []) list
;;



let list1 = [1; 2; 3; 4; 5; 6];;
let list2 = [6; 7; 8; 9; 10];;
let listEmpty = [];;

split3Rec(list1);;
split3Rec(list2);;
split3Rec(listEmpty);;
split3Tail(list1);;
split3Tail(list2);;
split3Tail(listEmpty);;
































(*let split3Tail list = 
    let rec split3Tail_helper list (acc1, acc2, acc3) = 
      match list with
      | x :: y :: z :: t -> split3Tail_helper t (x :: acc1, y :: acc2, z :: acc3)
      | _ -> (acc1, acc2, acc3) in
    let (rev1, rev2, rev3) = split3Tail_helper list ([], [], []) in
    let rec reverse lst acc =
      match lst with
      | [] -> acc
      | h :: t -> reverse t (h :: acc)
    in
    (reverse rev1 [], reverse rev2 [], reverse rev3 [])
  ;;*)
