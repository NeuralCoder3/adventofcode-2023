open Utils

(*
  This puzzle is short enough to theoretically do by hand.
  For part 2, a bidirectional search from both sides is efficient enough.
  Even faster (or to find the maximum), would be adapted binary search on bilinear functions.
*)

  
let (times,distances) =
  (* "inputs/6_0_1.txt" *)
  "inputs/6_1.txt"
  |> read_lines
  |> List.map (fun s ->
    s
    |> String.split_on_char ':' 
    |> List.tl
    |> List.hd
    |> String.split_on_char ' ' 
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
  )
  |> (fun xs -> (
    List.nth xs 0,
    List.nth xs 1
  ))


let races = List.combine times distances

let advantages (t,_d) = List.init t (fun k -> (t-k) * k)

let () =
  races
  |> List.map (fun (t,d) ->
    advantages (t,d)
    |> List.filter (fun s -> s > d)
    |> List.length
  )
  |> List.fold_left ( * ) 1
  |> dump_int 1

let time = String.concat "" (List.map string_of_int times) |> int_of_string
let distance = String.concat "" (List.map string_of_int distances) |> int_of_string

let min_time = first (fun k -> (time-k) * k > distance) 0
let max_time = time - first (fun k -> 
  let k' = time-k in
  (time-k') * k' > distance) 0

let () =
  (max_time - min_time + 1)
  |> dump_int 2

(* 
(* naive would also fast enough *)
let () =
  advantages (time,distance)
  |> List.filter (fun s -> s > distance)
  |> List.length
  |> dump_int 2 
*)