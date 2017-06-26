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


(* let str() = *)
(*     string "hi" *)

(* let private sys_char() = *)
(*     System.Char.IsDigit '1' *)

(* let char_member (c: char) = *)
(*     (c :> obj).ToString() *)

(* type Result<'T, string> with *)
(*       member r.get () : 'T = *)
(*          match r with *)
(*          | Ok s -> s *)
(*          | Error err -> failwith err; 0 *)

(* let get (r: Result<_,_>) = *)
(*      match r with *)
(*      | Ok s -> s *)
(*      | _ -> failwith "baa" *)

(* let explore() = *)
(*     let s = Set.empty *)
(*     let m = Map.empty *)
(*     m.Add (1, 2) *)

module N =
    type NTest2 =
        | N
        static member talk (t: NTest2) = "ntest2"
        member x.barf () = "ntest2b"

    let nt  () = ""

type Banana () =
    member x.eat() = 0


type Test2 =
    | Test2
    static member prt (t: Test2) = "test2"
    member x.print () = "test2b"

let tt() =
    let t = Test2
    Test2.prt t,
    t.print(),
    let b = Banana()
    b.eat(),
    N.NTest2.talk N.N,
    N.N.barf (),
    N.nt

let date () =
    let d = System.DateTime.Now
    d.AddDays 1.

let explore (l: int list) =
    l.[5]

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
    (*
exception SomeEx of string * int

let ex() =
    let x = SomeEx("banan", 12) :?> SomeEx
    x.Message
    *)
