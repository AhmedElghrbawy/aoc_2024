let read_lines fname =
  let ic = open_in fname in
  try
    let lines = In_channel.input_lines ic in
    close_in ic;
    lines
  with e ->
    close_in_noerr ic;
    raise e

let rec build_lists acc lines =
  match lines with
  | [] -> acc
  | (e1::e2::[])::ls' -> 
    let (acc1, acc2) = acc
    in build_lists ((int_of_string e1::acc1), (int_of_string e2::acc2)) ls'
  | _ -> raise (Failure "shouldn't happen")

let sorted_lists (l1, l2) =
  (List.sort compare l1, List.sort compare l2)

let rec compute_diff (l1, l2) =
  match (l1, l2) with
  | ([], []) -> 0
  | (x::xs', y::ys') -> abs (x - y) + compute_diff (xs', ys')
  | _ -> raise (Failure "shouldn't happen")



let get_ht xs =
  let ht = Hashtbl.create 10
  in 
    let rec count xs =
      match xs with
      | [] -> ht
      | x::xs' -> 
        let cur_val = Hashtbl.find_opt ht x
        in 
          match cur_val with
          | None -> Hashtbl.replace ht x 1; count xs'
          | Some i -> Hashtbl.replace ht x (i + 1); count xs'
    in count xs

let compute_sim xs ht =
  let rec compute xs =
    match xs with
    | [] -> 0
    | x::xs' ->
      let o = Hashtbl.find_opt ht x 
      in
        match o with
        | None -> compute xs'
        | Some v -> (x * v) + compute xs'
  in compute xs



let lists = read_lines "input.txt"
|> List.map (String.split_on_char ' ')
|> List.map (fun l -> List.filter (fun s -> s <> "") l)
|> build_lists ([], [])


let ans1 = lists
|> sorted_lists
|> compute_diff

let ans2 = get_ht (snd lists)
|> compute_sim (fst lists) 