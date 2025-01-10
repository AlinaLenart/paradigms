let pascalTriangle (n : int) : int list list =
  let arr = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to i do
      if j = 0 || j = i then
        arr.(i).(j) <- 1
      else
        arr.(i).(j) <- arr.(i-1).(j-1) + arr.(i-1).(j)
    done
  done;
  let result = ref [] in
  for i = n - 1 downto 0 do
    let row = ref [] in
    for j = i downto 0 do
      row := arr.(i).(j) :: !row
    done;
    result := !row :: !result
  done;
  !result

let pascalGift (n : int) : int list list =
  let triangle = pascalTriangle n in
  let result = ref [] in
  for i = n - 1 downto 0 do
    let side = ref [] in
    let x = ref (i + 2) in
    let y = ref (i + 1) in
    while !x < n && !y < List.length (List.nth triangle !x) do
      side := List.nth (List.nth triangle !x) !y :: !side;
      x := !x + 2;
      y := !y + 1
    done;
    let row = !side @ List.nth triangle i @ List.rev !side in
    result := !result @ [row]
  done;
  for i = 1 to n - 1 do
    let side = ref [] in
    let x = ref (i + 2) in
    let y = ref (i + 1) in
    while !x < n && !y < List.length (List.nth triangle !x) do
      side := List.nth (List.nth triangle !x) !y :: !side;
      x := !x + 2;
      y := !y + 1
    done;
    let row = !side @ List.nth triangle i @ List.rev !side in
    result := !result @ [row]
  done;
  !result

let print_box (box : int list list) =
  List.iter (fun row -> List.iter (fun x -> Printf.printf "%d " x) row; print_newline ()) box

let () =
  let n = 5 in
  let box = pascalGift n in
  print_box box
