module wip
open Fez.Core

(* let tryF f = *)
(*     try *)
(*         f () *)
(*     finally *)
(*         printfn "finally" *)
(* let xs = *)
(*     [1..10] *)
(*     |> List.map (fun x -> x * x) *)
(*     |> List.filter (fun x -> x % 2 = 0) *)
(*     |> List.choose (fun x -> if x % 3 = 0 then Some (x * 10) else None) *)
(*     |> List.sum *)

(*
 erlang terms:
     * cases with no values are lowercased and interpreted as atoms
     * cases with one value are interpreted as the value
     * cases with more than one value are interpreted as tuples
 *)

(* type IPrt = *)
(*     abstract member Prt: unit ->  string *)

(* type Obj = *)
(*     | O *)
(*     interface System.IDisposable with *)
(*         member x.Dispose() =  printfn "dispose" *)
(*     interface IPrt with *)
(*         member x.Prt() = "O" *)

(* let interfaces () = *)
(*     use o = O *)
(*     (o :> IPrt).Prt() *)

let str() =
    string "hi"

let sys_char() =
    System.Char.IsDigit '1'

let char_member (c: char) =
    c.ToString()
(*
type T() =
    abstract member Prt: unit ->  string

type T2() =
    inherit T()
    member x.Prt() = ""

type T3() =
    inherit T2()


let t =
    let t = T3()
    t.Prt()

[<ErlangTerm>]
type TimeUnit =
    | Second
    | Millisecond
    | Microsecond
    | Nanosecond
    | Native
    | Perf_counter
    | Integer of int //erased to value
    | SomeTuple of int * string //erased to tuple

[<ModCall("os", "system_time")>]
let os_system_time (opt : TimeUnit) =
    0L //dummy

let now () =
    os_system_time(Second),
    os_system_time(Integer 4)

let erlangTermRoundtrip () =


let estuff =
    function
    | Second -> "second"
    | Integer i -> sprintf "%i" i
    | SomeTuple (i, s) -> sprintf "%i %s" i s
    | _ -> "def"
*)
(* [<ModCall("erlang", "round")>] *)
(* let erlang_round (n: float) = *)
(*     0L *)

(* let t = *)
(*     "hey"B *)


(* type MyType = int * string *)

(*
type IPrt =
    abstract member Prt: unit ->  string

type Obj =
    | O
    interface System.IDisposable with
        member x.Dispose() =  printfn "dispose"
    interface IPrt with
        member x.Prt() = sprintf "%A" x

let bah () =
    use o = O
    (o :> IPrt).Prt()

*)
    (*
exception SomeEx of string * int

let ex() =
    let x = SomeEx("banan", 12) :?> SomeEx
    x.Message
    *)
