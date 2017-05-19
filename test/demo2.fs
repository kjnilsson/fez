module demo2

open Fez.Core

type YouSay =
    | Yes
    | Stop
    | Goodbye

type ISay =
    | No
    | GoGoGo
    | Hello

let prt() =
    match receive<YouSay>() with
    | Yes -> sprintf "%s" "yes"
    | Stop -> sprintf "%s" "stop"
    | Goodbye -> sprintf "%s" "goodbye"

let spawnYou () =
    let rec youFun iPid =
        match receive<ISay>() with
        | No ->
            iPid <! Yes
            youFun iPid
        | GoGoGo ->
            iPid <! Stop
            youFun iPid
        | Hello -> iPid <! Goodbye

    let iPid = self()
    spawn (fun () -> youFun iPid)

let sayNo youPid =
    youPid <! No
    prt()

let sayGo youPid =
    youPid <! GoGoGo
    prt()

let sayHello youPid =
    youPid <! Hello
    prt()
