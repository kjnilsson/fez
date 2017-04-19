module test2

let try_head l =
    match l with
    | h :: _ -> Some h
    | _ -> None

let try_something l =
    match l with
    | 1 :: _ :: h :: t when h > 2 -> Some (h)
    | _ -> None
(*
type my_rec = {name: string; age: int}

let age r = r.age

let bday ({age = age} as p) = {p with age = age + 1}

let try_head l =
    match l with
    | _ :: _ :: h :: t -> Some (h)
    | _ -> None

let rec sum n =
    match n with
    | 0 -> 0
    | 1 as n -> 1 + n
    | n -> sum (n-1) + n

let isZeroToFour x =
    match x with
    | x when x < 1 || x > 4 ->
        "no"
    | x -> "yes"

let tuple =
    function
    | (1, s) -> s
    | (2, s) -> s + s
    | (3, ("three" as s)) -> s + s
    | (_, _) -> "many"

    *)
