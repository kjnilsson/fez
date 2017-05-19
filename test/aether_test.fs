module aether_test

type RecordA =
    { B: RecordB }

    static member B_ =
        (fun a -> a.B), (fun b a -> { a with B = b})

 and RecordB =
    { Value: string }

    static member Value_ =
        (fun b -> b.Value), (fun value b -> { b with Value = value })


open Aether
open Aether.Operators

let test1() =

    let a =
        { B = { Value = "Hello World!" } }

    (* A lens, composed using Aether operators *)
    let avalue_ =
        RecordA.B_ >-> RecordB.Value_

    let a = Optic.set (RecordA.B_ >-> RecordB.Value_) "Goodbye World!" a
    (* Get the value using an existing lens *)
    Optic.get avalue_ a

    (* or... *)

    (* Set the value using a new lens *)
