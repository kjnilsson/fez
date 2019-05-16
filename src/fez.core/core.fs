[<AutoOpen>]
module Fez.Core

type Pid = P
type Port = Port
type Node = Node
type Reference = Reference
type Atom = Atom of string

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

let reference() = Reference

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

type ModCall(modu: string, func: string) =
    inherit System.Attribute()

type CaseStrategy =
    | LowerFirstCharLeaveAllUpper = 0

[<System.AttributeUsage(System.AttributeTargets.Class)>]
type ErlangTerm() =
    inherit System.Attribute()
    member val IncludeTagsWithTuples = false with get, set
    member val CaseStrategy = CaseStrategy.LowerFirstCharLeaveAllUpper with get, set

[<System.AttributeUsage(System.AttributeTargets.Interface)>]
type ErlangBehaviour() =
    inherit System.Attribute()

type Ref<'a> with
    member r.release () : 'a option =
        Some r.Value

type System.Lazy<'a> with
    member l.release () : 'a option =
        Some l.Value

[<ErlangTerm>]
type GenServerTimeout =
    | Timeout of int
    | Infinity

[<ErlangTerm>]
type From =
    | From of Pid * Reference

[<ErlangTerm>]
type CastResult<'TState> =
    | Noreply of 'TState
    | Stop  of Term * 'TState

[<ErlangTerm>]
type CallResult<'TState, 'TReply> =
    | Noreply of 'TState
    | Reply of 'TReply * 'TState
    | Stop  of Term * 'TState

[<ErlangBehaviour>]
type IGenServer<'TArgs, 'TState, 'TRequest>  =
    abstract member init : 'TArgs  -> 'TState
    abstract member handle_cast : 'TRequest -> 'TState ->
        CastResult<'TState>
    abstract member handle_call : 'TRequest -> From -> 'TState ->
        CallResult<'TState, 'TRequest>
    abstract member handle_info : Term -> 'TState ->
        CastResult<'TState>

module GenServer =
    [<ErlangTerm(IncludeTagsWithTuples = true)>]
    type ServerName =
        | Local of Atom
        | Global of Term
        | Via of Atom * Term

    [<ErlangTerm(IncludeTagsWithTuples = true)>]
    type StartLinkResult =
        | Ok of Pid
        | Error of Term

    let start_link<'TArgs> (name : ServerName) (modu : Atom) (args : 'TArgs) =
        Error

type MyGenServer private () =
    interface IGenServer<int, int, int> with
        member __.init args =
            args
        member __.handle_cast req state =
            CastResult.Noreply state
        member __.handle_call req from state =
            CallResult.Reply (55, state)
        member __.handle_info info state =
            CastResult.Noreply state
    static member start_link () = ""



/// binaries are represented as byte arrays and manipulated through the
/// Binary module

type Binary = byte array

module Binary =
    let at (pos :int) (subject : Binary) =
        0uy

    let part (pos :int) (len: int) (subject : Binary) =
        [||]

