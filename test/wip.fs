module wip
open Fez.Core

(* let tryF f = *)
(*     try *)
(*         f () *)
(*     finally *)
(*         printfn "finally" *)
let xs =
    [1..10]
    |> List.map (fun x -> x * x)
    |> List.filter (fun x -> x % 2 = 0)
    |> List.choose (fun x -> if x % 3 = 0 then Some (x * 10) else None)
    |> List.sum
(*
type Obj () =
    let age = 32
    member x.Age = age
    interface System.IDisposable with
        member x.Dispose() = ()

let bah () =
    let o = new Obj()
    1

    *)
    (*
exception SomeEx of string * int

let ex() =
    let x = SomeEx("banan", 12) :?> SomeEx
    x.Message
    *)
