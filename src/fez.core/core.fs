[<AutoOpen>]
module Fez.Core

type Pid = P
type Port = Port
type Node = Node

type Term =
    | Atom of string
    | Integer of int64
    | Float of float
    | List of Term list
    | BitString of byte []
    | String of string
    | MapT of Map<Term, Term>
    | Pid of Pid
    | Port of Port

type Dst =
    | DstPid of Pid
    | Port of Port
    | RegName of string


let receive<'Msg> () =
    // dummy, this is never called
    Unchecked.defaultof<obj> :?> 'Msg

let spawn (f : unit -> unit) : Pid =
    P

let self () = P

let send<'T> (dst: Pid) (msg: 'T) : unit =
    // in erlang `send` returns the msg
    // in fsharp this would just cause a warning so we
    // just return unit instead
    ()

let (<!) = send


(* let test = *)
(*     let d = self() |> DstPid *)
(*     d <! Atom *)


type ModCall(modu: string, func: string) =
    inherit System.Attribute()

type CaseStrategy =
    | LowerFirstCharLeaveAllUpper = 0

type ErlangTerm() =
    inherit System.Attribute()
    member val IncludeTagsWithTuples = false with get, set
    member val CaseStrategy = CaseStrategy.LowerFirstCharLeaveAllUpper with get, set


type Ref<'a> with
    member r.release () : 'a option =
        Some r.Value

type System.Lazy<'a> with
    member l.release () : 'a option =
        Some l.Value
