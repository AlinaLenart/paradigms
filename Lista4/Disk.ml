type 'a file = 
  | File of 'a  (*nazwa*)

type 'a folder = 
  | Folder of 'a * 'a folder list * 'a file list  (*nazwa, lista folderow, lista plikow*)


type 'a disk = 
  | Disk of string * 'a folder list * 'a file list  (*nazwa, lista folderow, lista plikow*)

(*czy powinno byc w nawiasach:  Disk of char * ('a folder list) * ('a file list)?*)


let rec path (disk: 'a disk) (name: string) : string option =
  let rec find_in_folder (folder: 'a folder) (name: string) (current_path: string) : string option =
    match folder with
    | Folder (folder_name, subfolders, files) ->
        let folder_path = current_path ^ "\\" ^ folder_name in
        if folder_name = name then Some (folder_path ^ "\\")
        else
          let file_path =
            if List.exists (fun (File f_name) -> f_name = name) files then
              Some (folder_path ^ "\\" ^ name)
            else
              None
          in
          match file_path with
          | Some path -> Some path
          | None ->
              let rec search_subfolders = function
                | [] -> None
                | folder::rest -> 
                    match find_in_folder folder name folder_path with
                    | Some path -> Some path
                    | None -> search_subfolders rest
              in
              search_subfolders subfolders
  in
  match disk with
  | Disk (disk_name, folders, files) ->
      let disk_path = disk_name ^ ":" in
      let folder_path =
        let rec search_top_folders = function
          | [] -> None
          | Folder (folder_name, _, _) as folder :: rest ->
              if folder_name = name then Some (disk_path ^ "\\" ^ folder_name ^ "\\")
              else
                match find_in_folder folder name disk_path with
                | Some path -> Some path
                | None -> search_top_folders rest
        in
        search_top_folders folders
      in
      match folder_path with
      | Some path -> Some path
      | None ->
          let file_path =
            if List.exists (fun (File f_name) -> f_name = name) files then
              Some (disk_path ^ "\\" ^ name)
            else
              None
          in
          file_path




let file1 = File "Holidays.png";;
let file2 = File "Invoice.txt" ;; 
let folder1 = Folder ("Pictures", [], [file1]);;  
let folder2 = Folder ("Documents", [], [file2]);;  

let disk1 = Disk ("C", [folder1; folder2], []);;  

let test1 = path disk1 "Holidays.png" ;;
print_endline ("Test 1: " ^ (Option.value test1 ~default:"File not found"));;

let test2 = path disk1 "Invoice.txt";;  
print_endline ("Test 2: " ^ (Option.value test2 ~default:"File not found"));;

let test3 = path disk1 "NonExistent.txt"  ;;
print_endline ("Test 3: " ^ (Option.value test3 ~default:"File not found"));;

let test4 = path disk1 "Documents" ;; 
print_endline ("Test 4: " ^ (Option.value test4 ~default:"Folder not found"));;

let test5 = path disk1 "NonExistentFolder"  ;;
print_endline ("Test 5: " ^ (Option.value test5 ~default:"Folder not found"));;
