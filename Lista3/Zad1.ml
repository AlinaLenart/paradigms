let ( !? ) coefficients = (*bierze liste*)
  let rec helper facs x power =
    match facs with 
    | [] -> 0.0
    | h :: t ->
        let term = h *. (x ** power) in
        term +. (helper t x (power +. 1.)) (*rekurencja nieogonowa *)
    in
  let evaluate x = helper coefficients x 0.0 
    in  
  evaluate (*zwraca funkcje => funckjonal*)
;;



let factors = [2.; 3.; 4.];; (* 2 + 3x + 4x^2 *)
let eval = !? factors;;
let result = eval 2.;; 

let empty_factors = [];;
let eval_empty = !? empty_factors;;
let empty_result = eval_empty 2.;;  
