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

    (*
exception SomeEx of string * int

let ex() =
    let x = SomeEx("banan", 12) :?> SomeEx
    x.Message
    *)
