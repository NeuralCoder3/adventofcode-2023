open Utils

let data = 
  (* "inputs/15_0_1.txt" *)
  "inputs/15_1.txt"
  |> read_lines
  |> List.hd
  |> String.split_on_char ','

let data1 = 
  data
  |> List.map explode
  |> List.map (List.map Char.code)

let hash xs =
  List.fold_left (fun c acc ->
    ((acc + c) * 17) mod 256
  ) 0 xs

let () =
  data1
  |> List.map hash
  |> List.fold_left (+) 0
  |> dump_int 1

let data2 = 
  data
  |> List.map (fun c -> 
    if String.contains c '=' then
      (
        List.hd (String.split_on_char '=' c), 
        Some (String.split_on_char '=' c |> List.tl |> List.hd |> int_of_string)
      )
    else
      (List.hd (String.split_on_char '-' c), None)
  )

let rec insert xs (l,v) = 
  match xs with
  | [] -> 
    (match v with
    | None -> []
    | Some v -> [(l,v)])
  | (l',v')::xs' -> 
    if l = l' then
      (
        match v with
        | None -> xs'
        | Some v -> (l,v)::xs'
      )
    else
      (l',v')::(insert xs' (l,v))


let rec update f xs k =
  match xs with
  | [] -> [(k,f None)]
  | (k',v)::xs' -> 
    if k = k' then
      (k,f (Some v))::xs'
    else
      (k',v)::(update f xs' k)

let () =
    data2
    |> List.fold_left (fun acc ((label,_) as c) -> 
      let code = explode label |> List.map Char.code |> hash in
      update (function
      | None -> insert [] c
      | Some xs -> insert xs c
      ) acc code
    ) []
    |> List.map (fun (code,vs) -> 
      vs
      |> List.mapi (fun i (_,v) -> 
        (i+1)*v*(1+code)
      )
      |> List.fold_left (+) 0
    )
    |> List.fold_left (+) 0
    |> dump_int 2