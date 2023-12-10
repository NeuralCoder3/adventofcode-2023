open Utils

let seqs = 
  (* "inputs/9_0_1.txt" *)
  "inputs/9_1.txt" 
  |> read_lines
  |> List.map (fun l ->
    l 
    |> String.split_on_char ' '
    |> List.map int_of_string
  )

let rec get_next xs =
  if List.for_all ((=) 0) xs then 0
  else
    let last = List.hd (List.rev xs) in
    last + get_next (List.map (fun (a,b) -> b-a) (zipNext xs))


let () =
  seqs
  |> List.map get_next
  |> List.fold_left (+) 0
  |> dump_int 1


let rec get_prev xs =
  if List.for_all ((=) 0) xs then 0
  else
    let first = List.hd xs in
    first - get_prev (List.map (fun (a,b) -> b-a) (zipNext xs))

let () =
  seqs
  |> List.map get_prev
  |> List.fold_left (+) 0
  |> dump_int 2