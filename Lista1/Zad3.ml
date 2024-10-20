(*Napisać funkcję insert przyjmującą listę, nowy element oraz pozycję, na którą ma on być
wstawiony i zwracającą nową listę zawierającą wstawiany element. Jeśli pozycja jest poza
zakresem, element należy wstawić na odpowiednim końcu listy*)

let rec insert_helper list element index count = 
  if count = index then
    element :: list  (*laczy listy*)
  else if list = [] then
    [element]  
  else
    List.hd list :: insert_helper (List.tl list) element index (count + 1)  (*rekurencja glebiej dolaczajaca liste*)


let insert list element index =
  if index < 0 then
    raise (Failure "BLAD: indeks tablicy < 0")  
  else
    insert_helper list element index 0  



let list1 = [1; 2; 3; 4; 5]  
let test1 = insert list1 0 0  (*  0 na indeks 0 *)

let list2 = [2; 3; 4]
let test2 = insert list2 10 6  (* 10 na indeks 6 (poza zakresem) *)

let test3 = insert list2 10 (-3)  (* 10 na indeks -3 (ujemny) *)
