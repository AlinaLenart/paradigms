let militaryMinutes (hours, minutes, am_pm) =
  if hours < 0 || hours > 12 then
    raise (Failure "BLAD: Godzina musi byc z zakresu 0-12")
  else if minutes < 0 || minutes >= 60 then
    raise (Failure "BLAD: Minuty musza byc z zakresu 0-59")
  else
    let military_hours =
      if am_pm = "AM" then
        if hours = 12 then 0  
        else hours
      else
        if hours = 12 then 12  
        else hours + 12  
    in
    Printf.sprintf "%2d : %02d" military_hours minutes  
;;

let () =
  print_endline (militaryMinutes (9, 5, "AM"));  (* "9 : 05" *)
  print_endline (militaryMinutes (12, 12, "PM")); (* "12 : 12" *)
  print_endline (militaryMinutes (12, 35, "AM"));  (* "0 : 35" *)
  print_endline (militaryMinutes (9, 5, "PM"));  (* "21 : 05" *)
  
  try
    print_endline (militaryMinutes (17, 1, "AM"));  
  with
  | Failure msg -> print_endline msg;  

  (*12 pm - noon*)
  (*12 am - midnight*)