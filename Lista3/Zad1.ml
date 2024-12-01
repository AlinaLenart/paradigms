let ( !? ) coefficients = (*bierze liste*)
  let rec helper facs x power =
    match facs with 
    | [] -> 0.0
    | h :: t ->
        let term = h *. (x ** power) in
        term +. (helper t x (power +. 1.)) (*rekurencja nieogonowa *)
  in
  fun x -> helper coefficients x 0.0
;;



let factors = [2.; 3.; 4.];; (*wielomian 2 + 3x + 4x^2 *)
let eval_poly = !? factors;;
let result = eval_poly 2.;;

(* DRUGIE PODEJSCIE   *)

let ( !? ) coefficients =
  fun x ->
    let calculate_term (index, acc) coeff =
      let term = coeff *. (x ** float_of_int index) in
      (index + 1, acc +. term)
    in
    let _, result = List.fold_left calculate_term (0, 0.0) coefficients in
    result
;;


let coefficients = [2.; 3.; 4.]  (*wielomian 2 + 3x + 4x^2 *)
let polynomial = !? coefficients;;

let result = polynomial 2.0;;  
let result2 = polynomial 1.0;; 


let empty_poly = !? [];; (*pusta lista*)
let empty_result = empty_poly 2.0;; 