type 'a file = 
  | File of { name: 'a } (*nazwa*)

type 'a folder = 
  | Folder of { 
      name: 'a;              
      subfolders: 'a folder list; 
      files: 'a file list      
    } (*nazwa, lista podfolderow, lista plikow*)

type 'a disk = 
  | Disk of { 
      name: string;              
      folders: 'a folder list;   
      files: 'a file list        
    }  (*nazwa, lista folderow, lista plikow*)


let rec path disk name =
  
  let rec find_in_files files name =
    match files with
    | [] -> None
    | File { name = f_name } :: rest -> 
        if f_name = name then Some f_name
        else find_in_files rest name
  in

  let rec find_in_folder folder name current_path =
    match folder with
    | Folder { name = folder_name; subfolders; files } ->
        let folder_path = current_path ^ "\\" ^ folder_name in
        match find_in_files files name with
        | Some f_name -> Some (folder_path ^ "\\" ^ f_name)
        | None ->
            let rec search_subfolders = function
              | [] -> None
              | subfolder :: rest ->
                  match find_in_folder subfolder name folder_path with
                  | Some path -> Some path
                  | None -> search_subfolders rest
            in
            search_subfolders subfolders
  in

  match disk with
  | Disk { name = disk_name; folders; files } ->
      let disk_path = disk_name ^ ":" in
      let rec search_top_folders = function
        | [] -> None
        | Folder { name = folder_name; _ } as folder :: rest ->
            if folder_name = name then Some (disk_path ^ "\\" ^ folder_name ^ "\\")
            else
              match find_in_folder folder name disk_path with
              | Some path -> Some path
              | None -> search_top_folders rest
      in
      match search_top_folders folders with
      | Some path -> Some path
      | None ->
          match find_in_files files name with
          | Some f_name -> Some (disk_path ^ "\\" ^ f_name)
          | None -> None

;;


type result = {
  folder_count: int;
  file_count: int;
}

let counts disk =
  let rec update_files result files_length =
    { folder_count = result.folder_count; file_count = result.file_count + files_length }
  in

  let rec update_folders result =
    { folder_count = result.folder_count + 1; file_count = result.file_count }
  in

  let rec count_folder folder result =
    match folder with
    | Folder { subfolders; files; _ } ->
        let result = update_folders result in
        let result = update_files result (List.length files) in
        let rec process_subfolders subfolders result =
          match subfolders with
          | [] -> result
          | subfolder :: rest ->
              let result = count_folder subfolder result in
              process_subfolders rest result
        in
        process_subfolders subfolders result
  in

  let Disk { folders; files; _ } = disk in
  let result = { folder_count = 0; file_count = 0 } in
  let result = update_files result (List.length files) in

  let rec process_folders folders result =
    match folders with
    | [] -> result
    | folder :: rest ->
        let result = count_folder folder result in
        process_folders rest result
  in

  process_folders folders result
;;


(* let get message = function
  | Some v -> v
  | None -> raise (Invalid_argument message) *)


let file1 = File { name = "Holidays.png" };;
let file2 = File { name = "Invoice.txt" };;
let folder1 = Folder { name = "Pictures"; subfolders = []; files = [file1] };;
let folder2 = Folder { name = "Documents"; subfolders = []; files = [file2] };;
let disk1 = Disk { name = "C"; folders = [folder1; folder2]; files = [] };;
let disk2 = Disk { name = "D"; folders = []; files = [] };;

let test1 = path disk1 "Holidays.png";;
let test2 = path disk1 "Invoice.txt";;
let test3 = path disk1 "Invisible.txt";;
let test4 = path disk1 "Documents";;
let test5 = path disk1 "InvisibleFolder";;

let testCounts1 = counts disk1;;
let testCounts2 = counts disk2;;








