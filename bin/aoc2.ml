open Utils

let get_or_default a o = 
  match o with
  | Some v -> v
  | None -> a

let data = 
  "inputs/2_1.txt"
  |> read_lines
  |> List.map (fun s ->
    (* let id = Scanf.scanf "Game %d: " (fun x -> x) s in *)
    let parts = String.split_on_char ':' s in
    let id = Scanf.sscanf (List.hd parts) "Game %d" (fun x -> x) in
    let games = 
      List.tl parts |> List.hd 
      |> String.trim 
      |> String.split_on_char ';' in
    (id, 
      games 
      |> List.map (fun s -> 
        let parts = String.split_on_char ',' s in
        let items = 
          parts 
          |> List.map String.trim
          |> List.map (fun s -> 
            let parts = String.split_on_char ' ' s in
            let count = List.hd parts |> int_of_string in
            let name = List.tl parts |> List.hd in
            (name, count)
          ) in
        (
            get_or_default 0 (List.assoc_opt "red" items),
            get_or_default 0 (List.assoc_opt "green" items),
            get_or_default 0 (List.assoc_opt "blue" items)
        )
      )
    )
  )


let () =
  let (maxr,maxg,maxb) = (12,13,14) in
  data
  |> List.filter (fun (_id,games) ->
    List.for_all (fun (r,g,b) -> 
      r <= maxr && g <= maxg && b <= maxb
    ) games
  )
  |> List.map fst
  |> List.fold_left (+) 0
  |> dump_int 1

let () =
    data 
    |> List.map (fun (id,games) ->
      (id, 
        List.fold_left (fun (r,g,b) (r',g',b') -> 
          (max r r', max g g', max b b')
        ) (0,0,0) games
      )
    )
    |> List.map (fun (id,(r,g,b)) -> (id, r*g*b))
    |> List.map snd 
    |> List.fold_left (+) 0
    |> dump_int 2
