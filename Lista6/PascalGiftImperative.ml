let pascalTriangle (n : int) : int array array =
  let triangle = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to i do
      if j = 0 || j = i then
        triangle.(i).(j) <- 1
      else
        triangle.(i).(j) <- triangle.(i - 1).(j - 1) + triangle.(i - 1).(j)
    done
  done;
  triangle
;;

let pascalGift (n : int) : int array array =
  let size = 2 * n - 1 in
  let gift = Array.make_matrix size n 0 in
  let triangle = pascalTriangle n in

  (* gorna czesc pudelka *)
  for i = n - 1 downto 0 do
    let side = Array.make n 0 in
    let x = ref (i + 2) in
    let y = ref (i + 1) in
    let idx = ref 0 in

    (* budowanie lewej strony *)
    while !x < n && !y < n do
      side.(!idx) <- triangle.(!x).(!y);
      idx := !idx + 1;
      x := !x + 2;
      y := !y + 1
    done;

    (* lewa strona wiersza *)
    for j = 0 to !idx - 1 do
      gift.(n - 1 - i).(j) <- side.(j)
    done;

    (* srodek wiersza *)
    for j = 0 to i do
      gift.(n - 1 - i).(!idx + j) <- triangle.(i).(j)
    done;

    (* prawa strona wiersza *)
    for j = 0 to !idx - 1 do
      gift.(n - 1 - i).(!idx + i + 1 + j) <- side.(j)
    done
  done;

  (* dolna czesc pudełka *)
  for i = 1 to n - 1 do
    let side = Array.make n 0 in
    let x = ref (i + 2) in
    let y = ref (i + 1) in
    let idx = ref 0 in

    (* budowanie lewej strony *)
    while !x < n && !y < n do
      side.(!idx) <- triangle.(!x).(!y);
      idx := !idx + 1;
      x := !x + 2;
      y := !y + 1
    done;

    (* lewa strona wiersza *)
    for j = 0 to !idx - 1 do
      gift.(n - 1 + i).(j) <- side.(j)
    done;

    (* srodek wiersza *)
    for j = 0 to i do
      gift.(n - 1 + i).(!idx + j) <- triangle.(i).(j)
    done;

    (* prawa strona wiersza *)
    for j = 0 to !idx - 1 do
      gift.(n - 1 + i).(!idx + i + 1 + j) <- side.(j)
    done
  done;

  gift
;;

(* let print_matrix (matrix : int array array) =
  let rows = Array.length matrix in
  for i = 0 to rows - 1 do
    let cols = Array.length matrix.(i) in
    for j = 0 to cols - 1 do
      if matrix.(i).(j) <> 0 then
        Printf.printf "%d " matrix.(i).(j)
    done;
    print_newline ()
  done *)


let gift5 = pascalGift 5;;
let gift3 = pascalGift 3;;
let gift1 = pascalGift 1;;
let gift0 = pascalGift 0;;

