open Utils

type kind = Spring | Ground | Unknown

(*
Simple version of nonogram
2^n possible combinations but n is small enough
*)

let data = 
  (* "inputs/12_0_1.txt" *)
  "inputs/12_1.txt"
  |> read_lines
  |> List.map (fun l ->
    let parts = String.split_on_char ' ' l in
    (
      List.nth parts 0 |> explode |> List.map (function '.' -> Ground | '#' -> Spring | '?' -> Unknown | _ -> failwith "Unknown char"), 
      List.nth parts 1 |> String.split_on_char ',' |> List.map int_of_string
    )
  )

let rec generate_layouts cs =
  match cs with
  | [] -> [[]]
  | Unknown::cs ->
    let layouts = generate_layouts cs in
    List.map (fun l -> Ground::l) layouts @ List.map (fun l -> Spring::l) layouts
  | c::cs ->
    List.map (fun l -> c::l) (generate_layouts cs)

let rec count xs c = 
  match xs with
  | [] -> if c = 0 then [] else [c]
  | Ground::xs -> 
    (if c = 0 then []
    else [c]) @ count xs 0
  | Spring::xs -> count xs (c+1)
  | Unknown::_ -> failwith "Unknown"

let _count_possibilities (c,xs) =
  generate_layouts c
  |> List.map (fun l -> count l 0)
  |> List.filter (fun l -> l = xs)
  |> List.length

let count_possibilities count_possibilities (c,xs) =
  let rec skip n xs =
    match n,xs with
    | 0,Unknown::xr 
    | 0,Ground::xr 
    | 0,([] as xr) -> true,xr
    | 0,Spring::_ -> false,[]

    | _,[] -> false,[]
    | _,Ground::_ -> false,[]

    | n,Unknown::xr 
    | n,Spring::xr -> skip (n-1) xr
  in
  match c,xs with
  | [],[] -> 1
  | [], _ -> 0
  | Ground::c',xr -> count_possibilities (c',xr)
  | Unknown::c', [] -> count_possibilities (c',[])

  | Spring::_, [] -> 0
  | Spring::_,k::xr -> 
    (match skip k c with
    | true,c -> count_possibilities (c,xr)
    | false,_ -> 0)

  | Unknown::c',xs ->
    count_possibilities (Ground::c',xs) +
    count_possibilities (Spring::c',xs)

let count_possibilities = memo_rec count_possibilities

let () =
  data
  |> List.map (fun (c,xs) -> count_possibilities (c,xs))
  |> List.fold_left (+) 0
  |> dump_int 1

let rec repeat sep k xs =
  if k = 0 then [] else
  if k = 1 then xs else
    xs @ sep @ repeat sep (k-1) xs

let () =
    data 
    |> List.map (fun (c,xs) ->
      (
        repeat [Unknown] 5 c,
        repeat [] 5 xs
      )
    )
    |> List.map (fun (c,xs) -> count_possibilities (c,xs))
    |> List.fold_left (+) 0
    |> dump_int 2