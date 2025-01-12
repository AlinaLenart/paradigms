let pascalTriangleF (n : int) : int list list =
  let rec next_row = function
    | [] -> []
    | row ->
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
      | [] -> build (i + 1) [[1]]  
      | last_row :: _ ->
          let r = next_row last_row in
          build (i + 1) (r :: acc)
  in
  List.rev (build 0 [])
;;


let get_at_index list index =
  let rec aux idx = function
    | [] -> failwith "Index out of bound"
    | x :: xs -> if idx = 0 then x else aux (idx - 1) xs
  in
  aux index list
;;

let pascalGiftF (n : int) : int list list =
  let trojkat = pascalTriangleF n in

  let rec collect idx idx2 =
    if idx < n && idx2 < List.length (get_at_index trojkat idx) then
      (get_at_index (get_at_index trojkat idx) idx2) :: collect (idx + 2) (idx2 + 1)
    else
      []
  in

  
  let rec loop_down i =
    if i < 0 then []
    else
      let dodatek = collect (i + 2) (i + 1) in
      let left_side = List.rev dodatek in
      let middle = get_at_index trojkat i in
      let right_side = dodatek in
      let row = left_side @ middle @ right_side in
      row :: loop_down (i - 1)
  in

  let rec loop_up i =
    if i >= n then []
    else
      let additional = collect (i + 2) (i + 1) in
      let left_side = List.rev dodatek in
      let middle = get_at_index trojkat i in
      let right_side = dodatek in
      let row = left_side @ middle @ right_side in
      row :: loop_up (i + 1)
  in

  
  let part1 = loop_down (n - 1) in
  let part2 = loop_up 1 in

  part1 @ part2
;;


let gift5 = pascalGiftF 5;;

