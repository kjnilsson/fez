[<AutoOpen>]
module Fez.Core

type Pid = Pid
type Port = Port
type Atom = Atom
type Node = Node

type Dst =
    | DstPid of Pid
    | Port of Port
    | RegName of Atom

let receive<'Msg> () =
    // dummy, this is never called
    Unchecked.defaultof<obj> :?> 'Msg

let spawn (f : unit -> unit) : Pid =
    Pid

let self () = Pid

let send<'T> (dst: Pid) (msg: 'T) : unit =
    ()

let (<!) = send


(* let test = *)
(*     let d = self() |> DstPid *)
(*     d <! Atom *)
