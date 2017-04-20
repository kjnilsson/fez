module wip

let try_match_a_list l =
    match l with
    | 1 :: _ :: h :: t when h > 2 -> Some h
    | _ -> None
