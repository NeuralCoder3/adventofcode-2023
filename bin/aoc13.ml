open Utils

let data = 
  (* "inputs/13_0_1.txt" *)
  (* "inputs/13_0_2.txt" *)
  "inputs/13_1.txt"
  |> read_lines
  |> List.map explode
  |> group_by (fun x -> x = [])

let reflect_row r xs =
  let a,b = takeDrop r xs in
  let b = List.rev b in
  let a_overflow = max 0 (List.length a - List.length b) in
  let b_overflow = max 0 (List.length b - List.length a) in
  let a = drop a_overflow a in
  let b = drop b_overflow b in
  List.combine a b
  |> List.map (fun (ar,br) -> List.combine ar br)
  |> List.flatten
  |> List.filter (fun (a,b) -> a <> b)
  |> List.length

(* Either but annotated *)
type mirror = 
  | Horizontal of int
  | Vertical of int
  | Nothing
  
let find_mirror part2 xs =
  let rec aux xs r =
    let n = List.length xs in
    if r >= n then None
    else if reflect_row r xs = (if part2 then 1 else 0) then Some r
    else aux xs (r+1)
  in
  match aux xs 1 with 
  | None -> 
    (match aux (transpose xs) 1 with
    | None -> Nothing
    | Some v -> Vertical v
    )
  | Some v -> Horizontal v

let solve part2 =
  data
  |> List.map (fun xs -> find_mirror part2 xs)
  |> List.mapi (fun i -> function
    | Nothing -> failwith (Printf.sprintf "Nothing at %d (row %d)" i 
      (List.fold_left (+) 0 (List.map (fun xs -> 1+List.length xs) (take i data))))
    | Horizontal r -> 100*r
    | Vertical r -> r
  )
  |> List.fold_left (+) 0
  |> dump_int (if part2 then 2 else 1)

let () =
  solve false;
  solve true

