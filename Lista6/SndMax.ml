let sndMax array =
  let length = Array.length array in
  if length < 2 then
    failwith "Array is too short"
  else
    let firstMax, secondMax =
      if array.(0) >= array.(1) then
        (array.(0), array.(1))
      else
        (array.(1), array.(0))
    in
    let firstMax = ref firstMax in
    let secondMax = ref secondMax in
  
    for i = 2 to length - 1 do
      let element = array.(i) in
      if element >= !firstMax then (
        secondMax := !firstMax;  
        firstMax := element     
      ) 
      else if element > !secondMax && element <> !firstMax then (
        secondMax := element      
      )
    done;
!secondMax
;;

let array1 = [|1; 2; 3; 4; 5; 6; 7; 8|] ;;
let array2 = [|0; 0; 0; 0|] ;;
let array3 = [|1; 2; 2; 0|] ;;
let arrayShort = [|1|];;

sndMax array1;;
sndMax array2;;
sndMax array3;;
sndMax arrayShort;;

