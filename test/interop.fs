module interop
open Fez.Core

//to represent atoms - will lowercase the case name
[<ErlangTerm>]
type TimeUnit =
    | Second
    | Millisecond
    | Microsecond
    | Nanosecond
    | Native
    | Perf_counter
    | Integer of int


[<ModCall("erlang", "system_time")>]
let erlang_system_time (opt : TimeUnit) =
    0L //dummy

let now1() = erlang_system_time TimeUnit.Millisecond
let now2() = erlang_system_time(Integer 4)