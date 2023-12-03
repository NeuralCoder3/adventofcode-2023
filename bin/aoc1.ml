open Utils

let numbers = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let words = ["zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] 
  
let lines =
  "inputs/1_1.txt"
  |> read_lines

(* shorter but not general enough for part 2 *)
let calibration1 = 
  lines
  |> List.map (fun l -> 
    l
    |> explode
    |> List.filter (fun c -> c >= '0' && c <= '9')
    |> (fun l -> [List.hd l; List.hd (List.rev l)])
    |> implode
    |> int_of_string
  )

let calibration numbers = 
  (* Aho–Corasick would be faster but not necessary *)
  let rec first_number s =
    match List.find_index (fun n -> String.starts_with s ~prefix:n) numbers with
    | Some i -> i mod 10
    | None -> first_number (String.sub s 1 (String.length s - 1))
  in
  let rec last_number s =
    match List.find_index (fun n -> String.ends_with s ~suffix:n) numbers with
    | Some i -> i mod 10
    | None -> last_number (String.sub s 0 (String.length s - 1))
  in
  lines
  |> List.map (fun l -> 
    try
    let first_number = first_number l in
    let last_number = last_number l in
    (first_number,last_number)
    with _ -> Printf.printf "error %s\n" l; (-1,-1)
  )

let () =
  calibration1
  |> List.fold_left (+) 0
  |> dump_int 1

let () =
  calibration numbers
  |> List.map (fun (a,b) -> 10*a+b)
  |> List.fold_left (+) 0
  |> dump_int 1

let () =
  calibration (numbers @ words)
  |> List.map (fun (a,b) -> 10*a+b)
  |> List.fold_left (+) 0
  |> dump_int 2