let pascalTriangleF (n : int) : int list list =
  let rec next_row = function
    | [] -> []
    | row ->
      (* Dodajemy 0 na początek i koniec, a potem sumujemy elementy parami *)
        let rec pair_sum l1 l2 =
          match l1, l2 with
          | x::xs, y::ys -> (x + y) :: pair_sum xs ys
          | _ -> []
        in
        let row_with_0_left  = 0 :: row in
        let row_with_0_right = row @ [0] in
        pair_sum row_with_0_left row_with_0_right
  in
  let rec build i acc =
    if i = n then acc
    else match acc with
      | [] -> build (i + 1) [[1]]  (* pierwszy wiersz = [1] *)
      | last_row :: _ ->
          let r = next_row last_row in
          build (i + 1) (r :: acc)
  in
List.rev (build 0 [])


(*[pascalGiftF n] – funkcyjna wersja budowania „pudełka Pascala”.
Generuje listę wierszy (każdy to [int list]), zgodnie z Twoim pseudokodem.

Używa:
- trójkąta Pascala (funkcja `pascalTriangleF`).
- rekurencji zamiast pętli for.
- reverse (dla `dodatek`) i `@` (sklejanie list).
*)
let pascalGiftF (n : int) : int list list =
let trojkat = pascalTriangleF n in

(* Funkcja pomocnicza zbierająca elementy z „dodatku”:
    Startuje od (tmpi, tmpk) i idzie w dół-prawo (tmpi += 2, tmpk += 1),
    dopóki istnieją odpowiednie elementy w trójkącie Pascala. *)
let rec collect tmpi tmpk =
  if tmpi < n && tmpk < List.length (List.nth trojkat tmpi) then
    (List.nth (List.nth trojkat tmpi) tmpk) :: collect (tmpi + 2) (tmpk + 1)
  else
    []
in

(* Rekurencyjna wersja pętli `for i = n-1 downto 0` *)
let rec loop_down i =
  if i < 0 then []
  else
    let dodatek = collect (i + 2) (i + 1) in
    let left_side  = List.rev dodatek in
    let middle     = List.nth trojkat i in
    let right_side = dodatek in
    let row = left_side @ middle @ right_side in
    row :: loop_down (i - 1)
in

(* Rekurencyjna wersja pętli `for i = 1 to n-1` *)
let rec loop_up i =
  if i >= n then []
  else
    let dodatek = collect (i + 2) (i + 1) in
    let left_side  = List.rev dodatek in
    let middle     = List.nth trojkat i in
    let right_side = dodatek in
    let row = left_side @ middle @ right_side in
    row :: loop_up (i + 1)
in

let part1 = loop_down (n - 1) in
let part2 = loop_up 1 in
part1 @ part2


(*********************************************
* 3) FUNKCJA POMOCNICZA DO WYŚWIETLENIA WYNIKU
*********************************************)

(** [print_box box] – Funkcja wypisująca listę wierszy (box) *)
let print_box (box : int list list) =
List.iter
  (fun row ->
      List.iter (fun x -> Printf.printf "%d " x) row;
      print_newline ();
  ) box


(*********************************************
* 4) PROSTY "MAIN" DO TESTU
*********************************************)

let () =
let n = 5 in
Printf.printf "=== Tworzymy 'pudełko' Pascala (n=%d) FUNKCYJNIE ===\n" n;

let box = pascalGiftF n in
print_box box;

Printf.printf "\n--- KONIEC ---\n"