open Utils

let lines = 
  (* "inputs/8_0_2.txt" *)
  "inputs/8_1.txt"
  |> read_lines

let instructions = List.hd lines 

let nodes =
  List.tl (List.tl lines)
  |> List.map (fun line -> 
    Scanf.sscanf line "%[0-9A-Z] = (%[0-9A-Z], %[0-9A-Z])" (fun n a b -> (n, (a,b)))
  )

let len = String.length instructions

let rec walk inst node n =
  if node = "ZZZ" then n
  else
    let graph = List.assoc node nodes in
    match String.get inst (n mod len) with
    | 'L' -> walk inst (fst graph) (n+1)
    | 'R' -> walk inst (snd graph) (n+1)
    | _ -> assert false


let () =
  walk instructions "AAA" 0
  |> dump_int 1


let rec walk2 inst node n =
  if String.get node 2 = 'Z' then n
  else
    let move = 
      if String.get inst (n mod len) = 'L' then
        fst 
      else
        snd
    in
    walk2 inst (move (List.assoc node nodes)) (n+1)

let lens = 
  nodes
  |> List.filter (fun (s,_) -> String.get s 2 = 'A')
  |> List.map (fun (s,_) -> (s, walk2 instructions s 0)) 

(*
   lcm is not enough in general (e.g. prefix before loop) -- we would need chinese remainder theorem
   there might even be more complex cycles out of two sections -- in this case, I would just use a CAS or ILP/SMT Solver
*)

let rec gcd a b =
  assert (a > 0 && b >= 0);
  if b = 0 then a else gcd b (a mod b)

let () = 
  List.fold_left (fun lcm (_,n) -> lcm * n/gcd lcm n) 1 lens
  |> dump_int 2
