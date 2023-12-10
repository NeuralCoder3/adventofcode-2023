open Utils

type tile = 
  | NS
  | EW
  | NE
  | NW
  | SE
  | SW
  | G
  | Start

let tile = function
  | '|' -> NS
  | '-' -> EW
  | 'L' -> NE
  | 'J' -> NW
  | '7' -> SW
  | 'F' -> SE
  | '.' -> G
  | 'S' -> Start
  (* | 'S' -> NE *)
  | _ -> failwith "invalid tile"

let data = 
  "inputs/10_1.txt"
  (* "inputs/10_0_1_0.txt" *)
  |> read_lines
  |> List.map explode
  |> List.map (List.map tile)

let sx,sy = 
  data
  |> List.mapi (fun y l -> List.mapi (fun x t -> ((x,y),t)) l)
  |> List.flatten
  |> List.find (fun ((_,_),t) -> t = Start)
  |> fst

let h = List.length data
let w = List.hd data |> List.length

type dir = N | E | S | W

let tile_to_char = function
  | NS -> ['N';'S']
  | EW -> ['E';'W']
  | NE -> ['N';'E']
  | NW -> ['N';'W']
  | SE -> ['S';'E']
  | SW -> ['S';'W']
  | _ -> []

let dir_to_char = function
  | N -> 'N'
  | E -> 'E'
  | S -> 'S'
  | W -> 'W'

let opposite = function
  | N -> S
  | E -> W
  | S -> N
  | W -> E

let connected curr dir next =
  let cs = tile_to_char curr in
  let ns = tile_to_char next in
  let ds = dir_to_char dir in
  List.mem ds cs && List.mem (dir_to_char (opposite dir)) ns

let moves = [
  (N,(0,-1));
  (E,(1,0));
  (S,(0,1));
  (W,(-1,0));
]

(* just hardcode it -- or try which tile connected to exactly two *)
let start_pipe = NE
let grid = List.map (List.map (function | Start -> start_pipe | t -> t)) data

let rec compute_distances dists n c dir (x,y) =
  if x < 0 || x >= w || y < 0 || y >= h then dists
  else 
    let p = List.nth (List.nth grid y) x in
    if n>0 && not (connected c dir p) then dists
    else
      (
      let change,new_dists = 
        match List.assoc_opt (x,y) dists with
        | None -> true,((x,y),n) :: dists
        | Some n' -> if n < n' then true,((x,y),n) :: List.remove_assoc (x,y) dists else false,dists
      in
      if not change then dists
      else
        List.fold_left (fun dists (d,(dx,dy)) ->
          compute_distances dists (n+1) p d (x+dx,y+dy)
        ) new_dists moves
      )

let dists = compute_distances [] 0 start_pipe N (sx,sy)

let () =
  dists
  |> List.fold_left (fun m (_,d) -> max m d) 0 
  |> dump_int 1



let path = dists |> List.map fst
let path_by_row =
  List.init h (fun y ->
    List.filter (fun (_,y') -> y = y') path
  )

(*
  right of path (not working in all cases)
  *2 + floodfill from outside
  edge crossing + line specialcase -- we chose this
*)

let () = 
  path_by_row
  |> List.map (fun row ->
    List.map (fun (x,y) ->
      let tile = List.nth (List.nth grid y) x in
      ((x,y),tile)
    ) row
    |> List.sort (fun ((x1,_),_) ((x2,_),_) -> compare x1 x2)
    |> List.fold_left (fun (count,lx,parity) ((x,_),t) ->
        ((if parity then 
            count + (x-lx-1) (* inside the pipes *)
          else
            count
        ),
        x,
        if t = NS || t = SE || t = SW then not parity else parity) (* ignore horizontal line segments *)
    ) (0,0,false)
  ) 
  |> List.fold_left (fun count (c,_,_) -> c+count) 0
  |> dump_int 2
  

