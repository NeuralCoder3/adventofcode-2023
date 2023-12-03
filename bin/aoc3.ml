open Utils

let data = 
  "inputs/3_1.txt"
  |> read_lines
  |> List.map explode

let is_symbol c = not (c = '.' || (c >= '0' && c <= '9'))

let symbols =
  List.mapi (fun y row ->
    List.mapi (fun x c -> 
      if is_symbol c then Some (c, x, y) else None
    ) row
  ) data
  |> List.flatten
  |> List.filter_map Fun.id

let neightbours =
  [
    (-1, -1); (-1, 0); (-1, 1);
    ( 0, -1);          ( 0, 1);
    ( 1, -1); ( 1, 0); ( 1, 1);
  ]

let numbers = 
  (* collect list of numbers and positions in a line *)
  let pattern = Str.regexp "[0-9]+" in
  List.mapi (fun y row ->
    let row_str = implode row in
    (* get all matches with indices *)
    let rec aux acc x =
      try
        let _ = Str.search_forward pattern row_str x in
        let x1 = Str.match_beginning () in
        let x2 = Str.match_end () in
        let num = Str.matched_string row_str |> int_of_string in
        aux ((num, ((x1,x2-1), y)) :: acc) x2
      with Not_found -> acc
    in
    aux [] 0
  ) data
  |> List.flatten

let adjacent = 
  List.map (fun ((_, sx, sy) as sym) ->
    let adjacent = List.filter_map
      (fun (num, ((x1,x2), y)) ->
        if List.exists (fun (dx,dy) ->
          let px = sx + dx in
          let py = sy + dy in
          px >= x1 && px <= x2 && py = y
        ) neightbours
        then Some num
        else None
      ) numbers
    in
    (sym, adjacent)
  ) symbols


let () =
  adjacent
  |> List.map (fun (_, adj) -> adj)
  |> List.flatten
  |> List.fold_left (+) 0
  |> dump_int 1

let gear_numbers = 
  adjacent
  |> List.filter_map (fun ((c,_,_), adj) ->
    match c, adj with
    | '*', [a;b] -> Some (a * b)
    | _ -> None
  )

let () =
  gear_numbers
  |> List.fold_left (+) 0
  |> dump_int 2

