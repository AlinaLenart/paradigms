let rec pascal_row prev_row =
  match prev_row with
  | [] -> [1]
  | [_] -> [1]
  | x :: y :: tail -> (x + y) :: pascal_row (y :: tail)

let rec pascal_triangle n =
  let rec build_triangle acc current_row i =
    if i = n then List.rev acc
    else build_triangle (current_row :: acc) (pascal_row current_row) (i + 1)
  in
  build_triangle [[1]] [1] 1

let rec extract_side triangle i x y =
  if x >= List.length triangle || y >= List.length (List.nth triangle x) then []
  else List.nth (List.nth triangle x) y :: extract_side triangle i (x + 2) (y + 1)

let build_row triangle i =
  let left_side = extract_side triangle i (i + 2) (i + 1) in
  let middle = List.nth triangle i in
  let right_side = List.rev left_side in
  left_side @ middle @ right_side

let pascal_gift n =
  let triangle = pascal_triangle n in
  let top_part = List.init n (fun i -> build_row triangle (n - 1 - i)) in
  let bottom_part = List.init (n - 1) (fun i -> build_row triangle (i + 1)) in
  top_part @ bottom_part

let print_box box =
  List.iter (fun row -> List.iter (fun x -> Printf.printf "%d " x) row; print_newline ()) box

let () =
  let n = 5 in
  let box = pascal_gift n in
  print_box box

