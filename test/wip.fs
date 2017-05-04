module wip
open fez.core

 (* if case_test *)
 (* if head = const(2) -> D:0 *)
 (* then *)
 (*     if head = const(3) -> *)
 (*         if case_test tail  -> D:1 (values: tail:head, tail:tail) *)
 (*         else D:2 *)
 (*     else D:2 *)


(* let recTest () = *)
(*     match receive<string>() with *)
(*     | "hi" as m-> Some m :b cerl*)
(*     | _ -> None *)

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

let prt_something s i =
    sprintf "something: %s  %i" s i

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
