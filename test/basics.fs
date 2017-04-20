module basics

let try_head l =
    match l with
    | h :: _ -> Some h
    | [] -> None

let try_match_a_list l =
    match l with
    | 1 :: _ :: h :: t when h > 2 -> Some h
    | _ -> None

type person = {name: string; age: int}

let make_person name age =
    {name = name
     age = age}

let age r = r.age

let have_birthday ({age = age} as p) = {p with age = age + 1}

let rec sum n =
    match n with
    | 0 -> 0
    | n -> sum (n-1) + n

let isOneToFour x =
    match x with
    | x when x < 1 || x > 4 ->
        "no"
    | x -> "yes"

//this function is translated as
//an arity/2 function
let tuple_matching t =
    match t with
    | (1, s) -> s
    | (2, s) -> s + s
    | (3, ("three" as s)) -> s + s
    | (_, _) -> "many"
