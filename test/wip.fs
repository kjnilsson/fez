module wip
open Fez.Core

let make_ints() =
    [0 .. 5]

let make_steps() =
    [0 .. 2 .. 6]

(* let yield_it x = *)
(*     seq { *)
(*         yield 1 *)
(*         yield 2 *)
(*         if x > 5 then *)
(*             yield x *)
(*     } *)

(* let try_head l = *)
(*     match l with *)
(*     | h :: _ -> Some h *)
(*     | [] -> None *)
 (* if case_test *)
 (* if head = const(2) -> D:0 *)
 (* then *)
 (*     if head = const(3) -> *)
 (*         if case_test tail  -> D:1 (values: tail:head, tail:tail) *)
 (*         else D:2 *)
 (*     else D:2 *)

(* type person = {name: string; age: int} *)
(* let make_person name age = *)
(*     {name = name *)
(*      age = age} *)

(* let age r = r.age *)

(* let have_birthday ({age = age} as p) = {p with age = age + 1} *)

(* type ISay = *)
(*     | Yes *)
(*     | Stay *)
(*     | Hello *)

(* type YouSay = *)
(*     | No *)
(*     | GoGoGo *)
(*     | Goodbye *)

(* let prt_msg() = *)
(*     match receive<YouSay>() with *)
(*     | No -> printfn "%s" "no" *)
(*     | GoGoGo -> printfn "%s" "gogogo" *)
(*     | Goodbye -> printfn "%s" "goodbye" *)

(* let rec you_fun iPid () = *)
(*     match receive<ISay>() with *)
(*     | Yes -> *)
(*         iPid <! No *)
(*         you_fun iPid () *)
(*     | Stay -> *)
(*         iPid <! GoGoGo *)
(*         you_fun iPid () *)
(*     | Hello -> iPid <! Goodbye *)

(* let hello_hello () = *)
(*     let iPid = self() *)
(*     let youPid = you_fun iPid |> spawn *)

    (* youPid <! Yes *)
    (* youPid <! Stay *)
    (* youPid <! Hello *)

    (* prt_msg() *)
    (* prt_msg() *)
    (* prt_msg() *)


(* let cube a b c = *)
(*     a * b * c *)

(* let mkCube1 a b = *)
(*     let ca = cube a *)
(*     ca b *)





(* type Maybe = *)
(*     | Ok of string *)
(*     | Err *)

(* let isErr = *)
(*     function *)
(*     | Ok "error" -> "ok_err" *)
(*     | Ok msg -> msg *)
(*     | Err -> "yes" *)

(* let try_match_a_list l = *)
(*     match l with *)
(*     | [1;_]  -> Some 1 *)
(*     | _ -> None *)
(* let fixed_len_list l = *)
(*     match l with *)
(*     | [1;_] -> Some 1 *)
(*     | [99;_;97;x] -> Some x *)
(*     | _ -> None *)

(* let prt_something s i = *)
(*     sprintf "something: %s  %i" s i *)

(* let try_match_a_list l = *)
(*     match l with *)
(*     | 1 :: _ :: h :: _ when h > 2 -> Some h *)
(*     | _ -> None *)
(* type Result = *)
(*     | JustOk *)
(*     | NotOk *)
(*     | Ok of int *)
(*     | Error of int * string *)

(* let makeRestlt i = *)
(*     if i > 100 then *)
(*         Ok i *)
(*     elif i < 0 then *)
(*         NotOk *)
(*     else JustOk *)

(* let handleResult r = *)
(*     match r with *)
(*     | JustOk -> 0 *)
(*     | NotOk -> 0 *)
(*     | Ok i -> i *)
(*     | Error (i, s) -> i *)
(* let listMap l = *)
(*     match List.map (fun x -> x +1) l with *)
(*     | 2 :: _ as l -> Some l *)
(*     | 3 :: f :: t -> Some t *)
(*     | _ -> None *)
