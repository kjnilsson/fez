module F

let (|Item|_|) = Map.tryFind

let findIt =
    function
    | Item "my_key" v -> v
    | Item "your_key" v -> v
    | _ -> "not ok"

type MyDU =
    | Case1 of int
    | Case2 of string

let makeMyDU () = Case1 42

let toString =
    function
    | Case1 n -> sprintf "%i" n
    | Case2 s -> s




open Fez.Core
(* let f a b = a + b *)

(* let f2 (a, b) = a + b *)

(* // who would do this? *)
(* let f3 (a, b) c = a + b + c *)

(* //but... *)
(* [<ModCall("F", "f4")>] *)
(* let f4 (a, b) c = a + b + c *)
