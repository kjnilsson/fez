module wip

type S = S
with
    static member (-) (s1, s2) =
        99

let custom_op_test() =
    let s1 = S
    let s2 = S
    s1 - s2

let set_Test() =
    let s1 = set [1;2;3]
    let s2 = set [2]
    Set.toList(s1 - s2)


(* open Fez.Core *)

(* [<ModCall("erlang", "put")>] *)
(* let put<'a, 'b> (k: 'a) (v: 'b) : 'b option = None *)

(* [<ModCall("erlang", "get")>] *)
(* let get<'a, 'b> (k: 'a) : 'b option = None *)
(* let refcell() = *)
(*     let v = ref 4 *)
(*     v := 5 *)
(*     v.Value, !v, v.release() *)

(* let so_lazy() = *)
(*     let l = lazy 5 *)
(*     //false, 5, true, 5, 5, undefined *)
(*     l.IsValueCreated, l.Force(), l.IsValueCreated, *)
(*         l.Value, l.release(), l.release() *)

(* type Thing = T *)
(*     with *)
(*     static member think (t: Thing) = "think" *)
(*     member t.thonk() = "thonk" *)

(* [<AutoOpen>] *)
(* module Inner = *)
(*     type Thing *)
(*         with static member thank (t: Thing) = "thank" *)

(* module Thing = *)
(*     let thunk (t: Thing) = *)
(*         Thing.think t *)

(* type List<'a> *)
(*     with *)
(*         static member prt (l : List<_>) = "list" *)
(*         member l.print() = "mlist" *)

(* let testThing () = *)
(*     Thing.thunk T, *)
(*     Thing.think T, *)
(*     Thing.thank T, *)
(*     T.thonk(), *)
(*     List.prt [], *)
(*     [].print() *)

(* let echo x = x *)
(* module Nested = *)
(*     type NestedType = *)
(*         | NT *)
(*         static member talk (t: NestedType) = "nestedtype.talk" *)
(*         member x.walk () = "nestedtype.walk" *)
(*     let nestedFunction () = *)
(*         "nestedfunction" *)

(*     let echo x = x *)

(*     module Nested2 = *)
(*         let echo x = x *)

(* type Test = *)
(*     | Test with *)
(*     static member prt (t: Test) = "test" *)

(* type Test2 = *)
(*     | Test2 *)
(*     static member prt (t: Test2) = "test2" *)

(* let nested_test () = *)
(*     Test.prt Test |> Nested.Nested2.echo |> Nested.echo |> echo *)

(* let nested_test2 () = *)
(*     Test2.prt Test2 |> Nested.Nested2.echo |> Nested.echo |> echo *)

(* let nested_test3() = *)
(*     let n = Nested.NestedType.NT *)
(*     Nested.NestedType.talk n, *)
(*     n.walk (), *)
(*     Nested.nestedFunction () *)
// TODO: generate the `get_Message` members for "fsharp" exceptions?

(* exception ExWithOverriddeMessage of int * string *)
(*     with *)
(*     override x.Message with get() = x.Data1 *)

(* exception BasicException of string *)

(* let explore () = *)
(*     try raise (exn "oops") with *)
(*     | :? ExWithOverriddeMessage as e -> *)
(*         e.Message *)
(*     | :? BasicException as e -> *)
(*         e.Message *)
(*     | e -> *)
(*         e.Message + "system.exception" *)

type O (s:  string) =
    let f = "A"
    member __.Test () = f + s


type O2 () =
    inherit O("O2")
    let f = "yey"
    member __.Test() = f

let testO() =
    let o = O2()
    o.Test()

type A () =
    let f = "A"
    let f2 = "A2"
    let f3 = f2 + "A3"
    abstract member Test: unit -> string
    default a.Test() = f
    member x.TestIt () =
        // call own abstact method
        x.Test()

type B () =
    inherit A()
    override x.Test () = "B"

type C () =
    inherit B()
    override x.Test () = "C"

type D () =
    inherit C()

let testOO () =
    let a = A()
    let b = B()
    let c = C()
    let d = D()
    // ("A", "B", "C", "C", "C", "C", "C")
    a.Test(), b.Test(), c.Test(), (c :> A).Test(), d.Test(), (d :> A).Test(), d.TestIt()


(* let ex() = *)
(*     let x = SomeEx(12, "msg") :?> SomeEx *)
(*     x.Message *)

(* let get_v = *)
(*     async { *)
(*         return self() } *)

(* let evt () = *)
(*     let evt = new Event<_>() *)
(*     Event.add (fun e -> printfn "hi") evt *)

(* let async_start_child_err () = *)
(*     async { *)
(*         let! p = Async.StartChild (async { *)
(*                                         failwith "bah" *)
(*                                         return self()}) *)
(*         return! p } *)
(*     |> Async.RunSynchronously *)

(* let async_run () = *)
(*     async { *)
(*         let! v = get_v *)
(*         return v} *)
(*     |> Async.RunSynchronously *)

(* let rarray() = *)
(*     let l = new ResizeArray<_>() *)
(*     l.Add 1 *)
(*     l *)

(* let query1 = *)
(*     query { *)
(*         for n in seq {0..10} do *)
(*             where (n > 5) *)
(*             select (n - 1) *)
(*     } *)

(* let bigints () = *)
(*     bigint 1 *)


(* #if FEZ *)
(* #else *)
(* let so_lazy() = *)
(*     let l = lazy 5 *)
(*     //false, 5, true, 5, 5, undefined *)
(*     l.IsValueCreated, l.Force(), l.IsValueCreated, *)
(*         l.Value, l.release(), l.release() *)
(* #endif *)


(* let send_receive() = *)
(*     (self()) <! (1, "hi") *)
(*     match receive<int * string>() with *)
(*     | 1, s -> s *)
(*     | n, _ -> string n *)


(* let results () = *)
(*     let r = Ok 1 *)
(*     let er = Error "blah" *)
(*     let r = Result.bind (fun x -> Ok (x + 1)) r *)
(*     let r = Result.map ((+) 2) r *)
(*     let er = Result.mapError String.length er *)
(*     r, er, r *)

(* [<AbstractClass>] *)
(* type SomeT() = *)
(*     abstract Prt: unit -> string *)

(* type T2() = *)
(*     inherit SomeT() *)
(*     override x.Prt() = "" *)

(* type T3() = *)
(*     inherit T2() *)

(* let t = *)
(*     let t = T3() *)
(*     t.Prt() *)

(*

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

