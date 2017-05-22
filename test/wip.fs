module wip
open Fez.Core

(* let add a b = a + b *)
(* let addFive = add 5 *)
(* let addSix x = addFive x + 1 *)
(* let addSeven = add 2 >> addFive *)
(* let addEight = add 3 << addFive *)
(* let make_ints() = *)
(*     [0 .. 5] *)

(* let make_steps() = *)
(*     [0 .. 2 .. 6] *)

(* let seq_expr () = *)
(*     let (height, width) = (10, 10) *)
(*     seq { for row in 0 .. width - 1 do *)
(*              for col in 0 .. height - 1 do *)
(*                yield (row, col, row*width + col) *)
(*         } *)
(*     |> Seq.toList *)

(* let mkAdd n (add: int -> int -> int) = *)
(*     (1* let rec add = *1) *)
(*     (1*     fun x -> fun n -> n + x *1) *)
(*     add n *)

let call =
    let adders = (fun a -> a + 5), (fun a b -> a + b)
    let add = snd adders
    add 4 5

(* let inner_fun (l : int list) = *)
(*     let folder c s a = c + s + a *)
(*     List.fold (folder 3) 0 l *)

(* type B  = { label: string } with *)
(*     static member show x = x.label *)
(*     static member make x = {label = x} *)

(* let inline show< ^T when ^T : (static member show : ^T -> string)> (x:^T) : string = *)
(*    (^T : (static member show : ^T -> string) (x)) *)

(* let echo x = x *)
(* module Nested = *)
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


(* let inner_rec l = *)
(*     let rec filter acc = *)
(*         function *)
(*         | [] -> acc *)
(*         | h :: tail when h > 5 -> *)
(*             filter acc tail *)
(*         | h :: tail -> *)
(*             filter (h :: acc) tail *)
(*     filter [] l *)

(* let for_expr (nums : int list) = *)
(*     [ *)
(*     for n in nums do *)
(*         printfn "squared %i" (n*n) *)
(*         yield n*n *)
(*     ] *)
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
