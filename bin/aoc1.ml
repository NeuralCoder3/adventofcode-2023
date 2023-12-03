open Utils

let data = 
  (* "inputs/1_1.txt" *)
  "inputs/1_0_1.txt"
  |> read_lines
  |> List.map (fun l -> 
    l
    |> explode
    |> List.filter (fun c -> c >= '0' && c <= '9')
    |> (fun l -> [List.hd l; List.hd (List.rev l)])
    |> implode
    |> int_of_string
  )

(* let () = 
  data
  |> List.iter (fun i -> Printf.printf "%d\n" i) *)



let () =
  data
  |> List.fold_left (+) 0
  |> dump_int 1




let numbers = ["zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine";
"0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
(* let numbers = List.map Str.regexp numbers *)

let data = 
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
    
  "inputs/1_1.txt"
  (* "inputs/1_0_2.txt" *)
  |> read_lines
  |> List.map (fun l -> 
    try
    let first_number = first_number l in
    let last_number = last_number l in
    (first_number,last_number)
    with _ -> Printf.printf "error %s\n" l; (-1,-1)

    (* let firstpos = 
    List.filter_map (fun n -> 
      if Str.string_match n l 0 then
      Some (Str.match_beginning ())
      else None
    ) numbers in
    let (_,_,first_number) = 
      List.fold_left (fun (i,p,n) pos ->
        if pos < p then (i+1,pos, n)
        else (i+1,p,n)
      ) (0,10000,-1) firstpos in
    let lastpos =
    List.filter_map (fun n -> 
      if Str.string_match n l 0 then
      Some (Str.match_end ())
      else None
    ) numbers in
    let (_,_,last_number) = 
      List.fold_left (fun (i,p,n) pos ->
        if pos > p then (i+1,pos, n)
        else (i+1,p,n)
      ) (0,-1,-1) lastpos in
    (first_number,last_number) *)
  )

(* let () =
  data
  |> List.iter (fun (a,b) -> Printf.printf "%d %d\n" a b) *)

let () =
  data
  |> List.map (fun (a,b) -> 10*a+b)
  |> List.fold_left (+) 0
  |> dump_int 2