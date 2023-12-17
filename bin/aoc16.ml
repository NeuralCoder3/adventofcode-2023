open Utils

type cell = Empty | Horizontal | Vertical | TL2BR | BL2TR
type direction = Up | Down | Left | Right
let offset = [
  Up, (0,-1);
  Down, (0,1);
  Left, (-1,0);
  Right, (1,0);
]

let grid = 
  (* "inputs/16_0_1.txt" *)
  "inputs/16_1.txt"
  |> read_lines
  |> List.map explode
  |> List.map (List.map (function
    | '.' -> Empty
    | '-' -> Horizontal
    | '|' -> Vertical
    | '\\' -> TL2BR
    | '/' -> BL2TR
    | _ -> failwith "invalid input"
  ))

let height = List.length grid
let width = List.length (List.hd grid)

let move (x,y) = function
  | Up -> (x,y-1)
  | Down -> (x,y+1)
  | Left -> (x-1,y)
  | Right -> (x+1,y)


let print_visited m =
  let m = List.map snd m in
  List.iteri (fun y row ->
    List.iteri (fun x c ->
      if List.mem (x,y) m then
        print_char '#'
      else
        print_char (match c with
          | Empty -> '.'
          | Horizontal -> '-'
          | Vertical -> '|'
          | TL2BR -> '\\'
          | BL2TR -> '/'
        )
    ) row;
    print_newline ()
  ) grid

let rec find_covered m dir (x,y) =
  if x < 0 || y < 0 || x >= width || y >= height then
    m
  else
    if List.mem (dir,(x,y)) m then m else
    let cell = List.nth (List.nth grid y) x in
    let m' = (dir,(x,y))::m in
    match cell, dir with
    | Empty, _ -> find_covered m' dir (move (x,y) dir)
    | Horizontal, (Up | Down) -> 
      let m'' = find_covered m' Left (move (x,y) Left) in
      find_covered m'' Right (move (x,y) Right)
    | Vertical, (Left | Right) ->
      let m'' = find_covered m' Up (move (x,y) Up) in
      find_covered m'' Down (move (x,y) Down)
    | Horizontal, (Left | Right) 
    | Vertical, (Up | Down) -> find_covered m' dir (move (x,y) dir)
    | TL2BR, Up -> find_covered m' Left (move (x,y) Left)
    | TL2BR, Right -> find_covered m' Down (move (x,y) Down)
    | TL2BR, Down -> find_covered m' Right (move (x,y) Right)
    | TL2BR, Left -> find_covered m' Up (move (x,y) Up)
    | BL2TR, Up -> find_covered m' Right (move (x,y) Right)
    | BL2TR, Right -> find_covered m' Up (move (x,y) Up)
    | BL2TR, Down -> find_covered m' Left (move (x,y) Left)
    | BL2TR, Left -> find_covered m' Down (move (x,y) Down)







let () =
  find_covered [] Right (0,0)
  |> List.map snd
  |> List.sort_uniq compare
  |> List.length
  |> dump_int 1


let () =
    [
      List.init width (fun x -> (Down, (x,0)));
      List.init width (fun x -> (Up, (x,height-1)));
      List.init height (fun y -> (Right, (0,y)));
      List.init height (fun y -> (Left, (width-1,y)));
    ]
    |> List.concat
    |> map_progress (fun (dir, pos) ->
      find_covered [] dir pos
    )
    |> List.map (fun visited ->
      visited
      |> List.map snd
      |> List.sort_uniq compare
      |> List.length
    )
    |> List.fold_left max 0
    |> dump_int 2

