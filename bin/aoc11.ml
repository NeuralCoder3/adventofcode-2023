open Utils

let grid = 
  "inputs/11_1.txt"
  |> read_lines
  |> List.map explode
  |> List.map (List.map (function '.' -> false | '#' -> true | _ -> failwith "invalid input"))


let duplicate_empty_rows data = List.fold_right (fun x acc -> 
  if List.for_all (fun x -> not x) x then x::x::acc
  else x :: acc
) data []

let rec transpose = function
  | [] | []::_ -> []
  | m -> List.map List.hd m :: transpose (List.map List.tl m)

let get_empty_rows grid = 
  grid 
  |> List.mapi (fun i row -> (i, List.for_all (fun x -> not x) row)) 
  |> List.filter (fun (_, b) -> b) |> List.map fst

let empty_rows = get_empty_rows grid
let empty_cols = get_empty_rows (transpose grid)

let part = 1
let space = if part = 1 then 2 else 1000000

let indices = 
  grid
  |> List.mapi (fun y row -> 
    List.mapi (fun x c -> ((x, y),c)) row
  ) 
  |> List.flatten
  |> List.filter (fun (_, c) -> c)
  |> List.map fst
  |> List.map ((fun (x, y) -> 
    (
      x+List.length(List.filter (fun x' -> x' < x) empty_cols)*(space-1),
      y+List.length(List.filter (fun y' -> y' < y) empty_rows)*(space-1)
    )
  ))

let distances = 
  List.map (fun ((x, y) as p1) -> 
    List.map (fun ((x', y') as p2) -> 
        (p1,p2,
          max 0 (abs (x - x') + abs (y - y'))
        )
    ) indices
  ) indices
  |> List.flatten

let () =
  distances
  |> List.map (fun (_, _, d) -> d)
  |> List.fold_left (+) 0
  |> (fun x -> x/2)
  |> dump_int 1
