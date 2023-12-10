open Utils

let data = 
  (* "inputs/7_0_1.txt" *)
  "inputs/7_1.txt"
  |> read_lines
  |> List.map (fun s ->
    let l = String.split_on_char ' ' s in
    (explode (List.hd l), List.nth l 1 |> int_of_string)
  )

(* type kind = 
    | Five       [5]
    | Four       [4;1]
    | FullHouse  [3;2]
    | Three      [3;1;1]
    | TwoPair    [2;2;1]
    | Pair       [2;1;1;1]
    | HighCard   [1;1;1;1;1]

  => uniquely defined by histogram of card counts
  *)

(* or we could pass the boolean across the functions *)
(* let part2 = false *)
let part2 = true

let histogram xs =
  let elements = List.sort_uniq compare xs in
  let count f xs = List.filter f xs |> List.length in
  List.map (fun e -> (e, count (fun x -> x = e) xs)) elements

let card_order = explode (if part2 then "J23456789TQKA" else "23456789TJQKA")

(* no match needed, lex on histogram is lex on type *)
let type_of_hand hand = 
  let joker = List.length (List.filter (fun c -> c = 'J') hand) in
  let hand = if part2 then List.filter (fun c -> c <> 'J') hand else hand in
  let hist = histogram hand |> List.map snd |> List.sort compare |> List.rev in
  if part2 then 
    ( if hist = [] then [5] else (joker+List.hd hist)::(List.tl hist) )
  else hist
let compare_type t1 t2 = compare t1 t2
let map_cards h = List.map (fun c -> Option.value (List.find_index (fun x -> x = c) card_order) ~default:~-1) h
let compare_cards h1 h2 = 
  compare
    (map_cards h1)
    (map_cards h2)

let compare_hand h1 h2 =
  match compare_type (type_of_hand h1) (type_of_hand h2) with
  | 0 -> compare_cards h1 h2
  | n -> n

let ranked =
  data
  |> List.sort (fun (k1, _) (k2, _) -> compare_hand k1 k2)

let () =
  ranked
  |> List.mapi (fun i (_, bid) -> (i+1) * bid)
  |> List.fold_left (+) 0
  |> dump_int (if part2 then 2 else 1)