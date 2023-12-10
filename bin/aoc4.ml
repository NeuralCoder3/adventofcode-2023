open Utils

let cards = 
  "inputs/4_1.txt"
  |> read_lines
  |> List.map (fun s -> 
    let parts = String.split_on_char ':' s in
    let id = List.hd parts in
    let id = Scanf.sscanf id "Card %d" (fun i -> i) in
    let data = List.hd (List.tl parts) in
    let parts = Str.split (Str.regexp "|") data in
    let winning = List.hd parts in
    let own = List.hd (List.tl parts) in
    let process s = 
      s
      |> String.split_on_char ' '
      |> List.filter (fun s -> s <> "")
      |> List.map int_of_string
    in
    (id, process winning, process own)
  )

let matches = 
  cards
  |> List.map (fun (id, winning, own) -> 
    let count = own 
      |> List.filter (fun i -> List.mem i winning)
      |> List.length
    in
    (id,count)
  )

let rec pow a b = if b = 0 then 1 else a * pow a (b - 1)

let points =
  matches
  |> List.map (fun (_, count) -> 
    if count = 0 then 0 else pow 2 (count - 1)
  )

(* 
   a range tree would be faster 
   alternatively, a list of running streaks might be interesting
*)
let scratchcard_count =
  let lookup m k = List.assoc_opt k m |> Option.value ~default:1 in
  let update m (k,v) = (k,v) :: List.filter (fun (k2,_) -> k <> k2) m in
  let rec aux m = function
    | [] -> 0
    | (id, count) :: t -> 
      (*
        update i+1..i+count + k
      *)
      let k = lookup m id in
      let m = 
        List.init count (fun i -> 
          let lid = id + i + 1 in
          let lk = lookup m lid in
          (lid, lk + k)
        )
        |> List.fold_left update m
      in
      k + aux m t
  in
  aux [] matches

let () =
  points
  |> List.fold_left (+) 0
  |> dump_int 1

let () =
  scratchcard_count
  |> dump_int 2