let nextRow =
  let pairSum l1 l2 =
    let rec helper acc l1 l2 =
      match l1, l2 with
      | x :: xs, y :: ys -> helper ((x + y) :: acc) xs ys
      | _ -> List.rev acc
    in
    helper [] l1 l2
  in
  fun row ->
    let rowLeftZeros = 0 :: row in
    let rowRightZeros = row @ [0] in
    pairSum rowLeftZeros rowRightZeros
;;

let generateRows =
  let rec helper idx triangle n =
    if idx = n then List.rev triangle
    else match triangle with
      | [] -> helper (idx + 1) [[1]] n
      | lastRow :: _ ->
          let newRow = nextRow lastRow in
          helper (idx + 1) (newRow :: triangle) n
  in
  fun n -> helper 0 [] n
;;

let pascalTriangle =
  fun n -> generateRows n
;;

(*pobieranie elementu z listy z indeksu*)
let getElementAtIndex =
  let rec helper idx list =
    match list with
    | [] -> failwith "Index out of bound"
    | x :: xs -> 
      if idx = 0 then x 
      else helper (idx - 1) xs
  in
  fun list index -> helper index list
;;

(*potrzebne do edgeElements*)
let collectElements =
  let rec helper acc x y n triangle = (*x to index wiersza, y to index kolumny*)
    if x < n && y < List.length (getElementAtIndex triangle x) then
      helper ((getElementAtIndex (getElementAtIndex triangle x) y) :: acc)
          (x + 2) (y + 1) n triangle
    else
      List.rev acc
  in
  fun x y n triangle -> helper [] x y n triangle
;;

(*gorna czesc prezentu*)
let buildTop =
  let rec helper acc i triangle n =
    if i < 0 then List.rev acc
    else
      let edgeElements = collectElements (i + 2) (i + 1) n triangle in
      let leftSide = List.rev edgeElements in
      let middle = getElementAtIndex triangle i in
      let rightSide = edgeElements in
      let fullRow = leftSide @ middle @ rightSide in
      helper (fullRow :: acc) (i - 1) triangle n
  in
  fun n triangle -> helper [] (n - 1) triangle n
;;

(*dolna czesc prezent*)
let buildBottom =
  let rec helper acc i triangle n =
    if i >= n then List.rev acc
    else
      let edgeElements = collectElements (i + 2) (i + 1) n triangle in
      let leftSide = List.rev edgeElements in
      let middle = getElementAtIndex triangle i in
      let rightSide = edgeElements in
      let fullRow = leftSide @ middle @ rightSide in (*polaczenie kazdej czesci w jedna liste*)
      helper (fullRow :: acc) (i + 1) triangle n
  in
  fun n triangle -> helper [] 1 triangle n
;;

let pascalGift =
  fun n ->
    let triangle = pascalTriangle n in
    let topPart = buildTop n triangle in
    let bottomPart = buildBottom n triangle in
    topPart @ bottomPart
;;

let gift5 = pascalGift 5;;
let gift4 = pascalGift 4;;
let gift3 = pascalGift 3;;
let gift2 = pascalGift 2;;
let gift1 = pascalGift 1;;
let gift0 = pascalGift 0;;



