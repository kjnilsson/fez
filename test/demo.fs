module demo
open Fez.Core

type Msg =
    | Add of int
    | Emit of Pid

let start () =
    let rec adder n =
        match receive<Msg>() with
        | Add i ->
            adder (n+i)
        | Emit pid ->
            pid <! n
            adder n
    spawn (fun () -> adder 0)

let add pid n =
    pid <! (Add n)

let emit pid =
    pid <! (Emit (self()))
