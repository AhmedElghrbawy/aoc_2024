let read_lines fname =
  let ic = open_in fname in
  try
    let lines = In_channel.input_lines ic in
    close_in ic;
    lines
  with e ->
    close_in_noerr ic;
    raise e

type slope = Init | Inc | Dec 

let is_safe xs =
  let rec is_safe xs slope_acc =
    match xs with
    | x::y::xs' -> 
      let diff = x - y in
       abs diff >= 1 && abs diff <= 3 && 
        (match slope_acc with
        | Init when diff < 0 -> is_safe (y::xs') Inc
        | Init when diff > 0 -> is_safe (y::xs') Dec
        | Init -> false
        | Inc when diff < 0 -> is_safe (y::xs') Inc
        | Inc -> false
        | Dec when diff > 0 -> is_safe (y::xs') Dec
        | Dec -> false)
    | x::[] -> true
    | _ -> raise (Failure "shouldn't happen")
  in is_safe xs Init



let lists = read_lines "input.txt"
|> List.map (String.split_on_char ' ')
|> List.map (fun l -> List.map int_of_string l)


let ans = lists 
|> List.map is_safe
|> List.fold_left (fun acc x -> if x then acc + 1 else acc) 0 

