open Utils

type map_range = {
  source_start: int;
  target_start: int;
  length: int;
}

type 'a map = {
  source: string;
  target: string;
  mapping: 'a list;
}

let seeds,maps = 
  let groups = 
    (* "inputs/5_0_1.txt" *)
    "inputs/5_1.txt"
    |> read_lines
    |> group_by (fun x -> x = "")
  in
  let seeds = 
    List.hd groups 
    |> List.hd
    |> String.split_on_char ' '
    |> List.tl
    |> List.map int_of_string
  in
  let mapping = 
    List.tl groups
    |> List.map (fun x -> 
      let name = List.hd x in
      (* \w-to\w map: *)
      let (source, target) = Scanf.sscanf name "%[a-z]-to-%[a-z] map:" (fun a b -> (a,b)) in
      let ranges = 
        List.tl x
        |> List.map (fun s -> 
          Scanf.sscanf s "%d %d %d" (
            fun target_start source_start length -> 
              { source_start; target_start; length }
          )
        )
      in
      { source; target; mapping = ranges }
    )
  in
  (seeds, mapping)

let map map v =
  let range = List.find_opt (fun x -> x.source_start <= v && v < x.source_start + x.length) map.mapping in
  match range with
  | None -> v
  | Some r -> r.target_start + v - r.source_start

let find_cat destcat (f,mapper) seeds =
  snd (repeatUntil (fun (cat,_values) ->
    cat = destcat
  )
  (fun (cat,values) ->
    let m = List.find (fun x -> x.source = cat) maps in
    (m.target, f (List.map (mapper m) values))
  )
  ("seed", seeds))

let () = 
    find_cat "location" (Fun.id,map) seeds
    |> List.fold_left min max_int
    |> dump_int 1




(*
   Ideas:
   - [s,l) -> [s,s+1,..., s+l-1] => too many elements (well possible but takes a few minutes)
   - invert map, `first` location => could be slow (10^8 elements)
   - map ranges => many corner cases
   - compute overlaps
*)

let seed_ranges = 
  let rec ranges = function
    | [] -> []
    | a::b::rest -> (a,b)::(ranges rest)
    | _ -> failwith "odd number of seeds"
  in
  ranges seeds
  |> List.map (fun (s,l) -> 
    (* left,right (excluding right) *)
    (s, s+l)
  )

type range = {
  left: int;
  right: int;
  offset: int;
}

let maps = 
  maps
  |> List.map (fun m -> 
    let ranges = 
      m.mapping
      |> List.map (fun r -> 
        { 
          left = r.source_start; 
          right = r.source_start + r.length;
          offset = r.target_start - r.source_start;
        }
      )
    in
    {
      source = m.source;
      target = m.target;
      mapping = ranges;
    }
  )


let apply map spans =
  let fit_range r (lv,rv) =
    let il = max r.left lv in
    let ir = min r.right rv in
    if il < ir then
      (* mapped *)
      [(il + r.offset, ir + r.offset)],
      (* unmapped *)
      [(lv,il); (ir,rv)]
    else
      (
        [],
        [(lv,rv)]
      )
  in
  List.fold_left (fun (mapped,unmapped) r ->
    let mus = List.map (fit_range r) unmapped in
    let us = List.concat (List.map snd mus) in
    let ms = List.concat (List.map fst mus) in
    let us = List.filter (fun (l,r) -> l < r) us in
    let ms = List.filter (fun (l,r) -> l < r) ms in
    (ms@mapped, us)
  ) ([],spans) map.mapping


let rec combine = function
  | (l1,r1)::(l2,r2)::rest when r1 >= l2 -> combine ((l1,max r1 r2)::rest)
  | (l1,r1)::(l2,r2)::rest -> (l1,r1)::(combine ((l2,r2)::rest))
  | x -> x

let () =
    List.fold_left (fun spans map ->
      let ms,us = apply map spans in
      ms @ us
      |> List.sort (fun (l1,_) (l2,_) -> compare l1 l2)
    ) seed_ranges maps
    |> List.map fst
    |> List.fold_left min max_int
    |> dump_int 2