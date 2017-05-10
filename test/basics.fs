module basics

//Application
let flip f a b = f b a

let try_head l =
    match l with
    | h :: _ -> Some h
    | [] -> None

let try_match_a_list l =
    match l with
    | 1 :: _ :: h :: _ when h > 2 -> Some h
    | _ -> None

let fixed_len_list l =
    match l with
    | [1;_] -> Some 1
    | [99;_;97;x] -> Some x
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


let add a b = a + b
let addFive = add 5
let addSix x = addFive x + 1
let addSeven = add 2 >> addFive
let addEight = add 3 << addFive

let strLen (s: string) = s.Length
let strLen2 s = String.length s
let listLen (s: List<_>) = s.Length

let addOneToAll l = List.map (fun x -> add x 1) l
let foldTest l = List.fold (fun s n -> s + n) 0 l
let foldTest2 l = List.fold add 0 l

let sort l = List.sort l

type TestDU =
    | JustOk
    | NotOk
    | Ok of int
    | Error of int * string

let makeDU i =
    if i > 100 then
        Ok i
    elif i < 0 then
        NotOk
    else JustOk

let handleTestDU r =
    match r with
    | JustOk -> 0
    | NotOk -> 0
    | Ok i -> i
    | Error (i, _) -> i

let prt_something s i =
    sprintf "prt: %s %i" s i

type ISay =
    | Yes
    | Stay
    | Hello

type YouSay =
    | No
    | GoGoGo
    | Goodbye

open Fez.Core

let prt_msg() =
    match receive<YouSay>() with
    | No -> sprintf "%s" "no"
    | GoGoGo -> sprintf "%s" "gogogo"
    | Goodbye -> sprintf "%s" "goodbye"

let rec you_fun iPid () =
    match receive<ISay>() with
    | Yes ->
        iPid <! No
        you_fun iPid ()
    | Stay ->
        iPid <! GoGoGo
        you_fun iPid ()
    | Hello -> iPid <! Goodbye

let hello_hello () =
    let iPid = self()
    let youPid = you_fun iPid |> spawn

    youPid <! Yes
    youPid <! Stay
    youPid <! Hello

    prt_msg(),
        prt_msg(),
            prt_msg()


let yield_it x =
    [
        yield 1
        if x > 5 then
            yield x
    ]

let make_ints() =
    [0 .. 3]

let make_steps() =
    [0 .. 2 .. 6]
