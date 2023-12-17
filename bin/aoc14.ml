open Utils

type rocks = 
  | Rock
  | Empty (* important: Rock<Empty *)
  | Fixed 

let data = 
  (* "inputs/14_0_1.txt" *)
  "inputs/14_1.txt"
  |> read_lines
  |> List.map explode
  |> List.map (List.map (function 
    | '.' -> Empty
    | '#' -> Fixed
    | 'O' -> Rock
    | _ -> failwith "invalid input"
  ))

let rec join_with sep xxs = 
  match xxs with
  | [] -> []
  | [xs] -> xs
  | x::xs -> x @ sep @ join_with sep xs

let group_by ?(keep=false) p xs : 'a list list =
  let rec aux g xs =
    match xs with
    | [] -> [List.rev g]
    | x :: xs ->
      if p x then (List.rev g)::aux (if keep then [x] else []) xs
      else aux (x::g) xs
  in
  aux [] xs


let move_left =
  List.map (fun row -> 
    row 
    |> group_by ((=) Fixed)
    |> List.map (List.sort compare)
    |> join_with [Fixed]
  )

let rotate xxs = 
  let xxs = transpose xxs in
  let xxs = List.map List.rev xxs in
  xxs



let print_data data = 
  data
  |> List.map (List.map (function 
    | Empty -> '.'
    | Fixed -> '#'
    | Rock -> 'O'
  ))
  |> List.map implode
  |> String.concat "\n"
  |> print_endline

let h = List.length data

let support_cost xxs =
  xxs
  |> iter rotate 1
  |> List.mapi (fun i row -> 
    (row
    |> List.filter ((=) Rock)
    |> List.length)
    * (h - i)
  )
  |> List.fold_left (+) 0

let () =
  data
  |> iter rotate 3
  |> move_left
  |> support_cost
  |> dump_int 1


let rec iter_memo f n x = 
  let tbl = [] in
  let rec aux x k tbl = 
    if k = n then x
    else 
      (* if encountered shorten n by period *)
      match List.assoc_opt x tbl with
      | Some s -> 
        let period = k - s in
        let rem = n - k in
        let rem = rem mod period in
        iter_memo f rem x
      | None ->
        let x' = f x in
        aux x' (k + 1) ((x, k)::tbl)
  in
  aux x 0 tbl


let cycle xxs =
  xxs 
  |> move_left (* north *)
  |> iter rotate 1 |> move_left (* east *)
  |> iter rotate 1 |> move_left (* south *)
  |> iter rotate 1 |> move_left (* west *)
  |> iter rotate 1 (* go back to north *)

let () =
  data 
  |> iter rotate 3
  |> iter_memo cycle 1000000000
  |> support_cost
  |> dump_int 2