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
let strConcat(sep: string, strList:List<string>) = String.concat sep strList
let hasAs str = String.exists (fun x -> x = 'A') str
let allAs str = String.forall (fun x -> x = 'A') str
let times8 str = String.replicate 8 str

let strToAs str = String.map (fun c -> 'A') str

let removeFirstTwo str = String.mapi (fun i c -> if i <= 1 then '_' else c) str
let removeAfterTwo str = String.mapi (fun i c -> if i >= 2 then '_' else c) str

let repeatIndexTimes n str = String.init n (fun i -> String.replicate i str)
let doubleChars str = String.collect (fun c -> string c + string c) str
let getMores str = String.collect (fun c -> "more") str

let toUpper str = String.map (fun c -> System.Char.ToUpper c) str
let toLower str = String.map (fun c -> System.Char.ToLower c) str

let charsAreLower l = List.map (fun c -> System.Char.IsLower c) l
let charsAreUpper l = List.map (fun c -> System.Char.IsUpper c) l
let charsToUpper l = List.map (fun c -> System.Char.ToUpper c) l
let charsToLower l = List.map (fun c -> System.Char.ToLower c) l
let charsAreDigit l = List.map (fun c -> System.Char.IsDigit c) l
let charsAreControl l = List.map (fun c -> System.Char.IsControl c) l
let charsAreLetter l = List.map (fun c -> System.Char.IsLetter c) l
let charsAreLetterOrDigit l = List.map (fun c -> System.Char.IsLetterOrDigit c) l
let charsAreSeparator l = List.map (fun c -> System.Char.IsSeparator c) l
let charsArePunctuation l = List.map (fun c -> System.Char.IsPunctuation c) l
let parseLatin1 str = System.Char.Parse str
// let tryParse str = System.Char.TryParse str

let addOneToAll l = List.map (fun x -> add x 1) l
let foldTest l = List.fold (fun s n -> s + n) 0 l
let foldTest2 l = List.fold add 0 l
let uniquify l = List.distinct l

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


let inner_fun (l : int list) =
    let folder c s a = c + s + a
    List.fold (folder 3) 0 l

let let_rec l =
    let rec filter acc =
        function
        | [] -> List.rev acc
        | h :: tail when h > 5 ->
            filter acc tail
        | h :: tail ->
            filter (h :: acc) tail
    filter [] l

type B  = { label: string } with
    static member show x = x.label
    static member make x = {label = x}

let inline show< ^T when ^T : (static member show : ^T -> string)> (x:^T) : string =
   (^T : (static member show : ^T -> string) (x))


let echo x = x
module Nested =
    let echo x = x
    module Nested2 =
        let echo x = x

type Test =
    | Test with
    static member prt (t: Test) = "test"

type Test2 =
    | Test2
    static member prt (t: Test2) = "test2"

let nested_test () =
    Test.prt Test |> Nested.Nested2.echo |> Nested.echo |> echo

let nested_test2 () =
    Test2.prt Test2 |> Nested.Nested2.echo |> Nested.echo |> echo

exception MyEx of string

let try_with_test () =
    try
        raise (MyEx "banana")
        "99"
    with
    | :? MyEx as e ->
        e.Data0

let (|Item|_|) = Map.tryFind

let empty_map () =
    Map.empty

let non_empty_map() =
    empty_map() |> Map.add "hi" "there"

let map_test m =
    match m with
    | Item "hi" v -> v
    | _ -> "banana"

let tryF f =
    try
        f ()
    finally
        printfn "finally"

type IPrt =
    abstract member Prt: unit ->  string

type Obj =
    | O
    interface System.IDisposable with
        member x.Dispose() =  printfn "dispose"
    interface IPrt with
        member x.Prt() = "O"

let interfaces () =
    use o = O
    (o :> IPrt).Prt()

//to represent atoms - will lowercase the case name
[<ErlangTerm>]
type TimeUnit =
    | Second
    | Millisecond
    | Microsecond
    | Nanosecond
    | Native
    | Perf_counter
    | Integer of int

[<ModCall("erlang", "system_time")>]
let erlang_system_time (opt : TimeUnit) =
    0L //dummy

let now () =
    erlang_system_time(TimeUnit.Millisecond),
    erlang_system_time(Integer 4)


[<ErlangTerm>]
type TestTerm =
    | Second
    | Integer of int
    | Tuple of int * int

let erlang_term_match =
    function
    | Second -> "second"
    | Integer i -> sprintf "%i" i
    | Tuple (a, b) -> sprintf "%i %i" a b

let just_string = string "a_string"
let just_char = string 'a'

let results () =
    let r = Result.Ok 1
    let er = Result.Error "blah"
    let r = Result.bind (fun x -> Result.Ok (x + 1)) r
    let r = Result.map ((+) 2) r
    let er = Result.mapError String.length er
    r, er

let operator_byte() =
    // [1uy;99uy;134uy;123uy]
    byte "1",
    byte 99,
    byte 1234566,
    byte 123.99

let mod_div_append_hash() =
    let modu = 10 % 5
    let div = 10 / 5
    let app = [] @ []
    modu, div, app, hash 55
