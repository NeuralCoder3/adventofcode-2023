open Utils

let data = 
  "inputs/3_1_0.txt"
  |> read_lines
  |> List.map explode

let is_symbol c =
  if c = '.' then false else 
    not(c >= '0' && c <= '9')

let symbols =
  List.mapi (fun y row ->
    List.mapi (fun x c -> 
      if is_symbol c then Some (c, x, y) else None
    ) row
  ) data
  |> List.flatten
  |> List.filter_map Fun.id

(* let () = 
  symbols
  |> List.iter (fun (i, j) -> 
    Printf.printf "%d %d\n" i j
  ) *)

let neightbours =
  [
    (-1, -1); (-1, 0); (-1, 1);
    ( 0, -1);          ( 0, 1);
    ( 1, -1); ( 1, 0); ( 1, 1);
  ]

let engine_pos = 
  List.map (fun (_,sx,sy) ->
    List.map (fun (dx, dy) ->
      (sx + dx, sy + dy)
    ) neightbours
  ) symbols
  |> List.flatten

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
        aux ((num, (x1,x2-1, y)) :: acc) x2
      with Not_found -> acc
    in
    aux [] 0
  ) data
  |> List.flatten

(* let () =
  numbers
  |> List.iter (fun (num, (x1,x2, y)) ->
    Printf.printf "%d-%d %d = %d\n" x1 x2 y num
  ) *)

(* let () =
  engine_pos
  |> List.iter (fun (x, y) ->
    Printf.printf "%d %d\n" x y
  ) *)

let parts =
  List.filter_map
    (fun (num, (x1,x2, y)) ->
      if List.exists (fun (ex, ey) ->
        ex >= x1 && ex <= x2 && ey = y
      ) engine_pos
      then Some num
      else None
    ) numbers

(* let () =
  parts
  |> List.iter (fun num ->
    Printf.printf "%d\n" num
  ) *)

let () =
  parts
  |> List.fold_left (+) 0
  |> dump_int 1

let adjacent = 
  List.map (fun (c, sx, sy) ->
    let adjacent = List.filter_map
      (fun (num, (x1,x2, y)) ->
        if List.exists (fun (dx,dy) ->
          let px = sx + dx in
          let py = sy + dy in
          px >= x1 && px <= x2 && py = y
        ) neightbours
        then Some num
        else None
      ) numbers
    in
    (c, sx, sy, adjacent)
  ) symbols

let gear_numbers = 
  adjacent
  |> List.filter_map (fun (c, _, _, adj) ->
    match c, adj with
    | '*', [a;b] -> Some (a * b)
    | _ -> None
  )

let () =
  gear_numbers
  |> List.fold_left (+) 0
  |> dump_int 2

