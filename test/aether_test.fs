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

let lens() =

    let a =
        { B = { Value = "Hello World!" } }

    (* A lens, composed using Aether operators *)
    let avalue_ =
        RecordA.B_ >-> RecordB.Value_


    printfn "lens %A" avalue_
    let s = Optic.set avalue_ "Goodbye World!"
    (* Get the value using an existing lens *)
    Optic.get avalue_ (s a)

    (* or... *)

    (* Set the value using a new lens *)
