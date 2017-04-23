module wip
open fez.core

(* let add a b = a + b *)
(* let addFive = add 5 *)
(* let addSix x = addFive x + 1 *)
(* let addSeven = add 2 >> addFive *)
(* let addSeven2 = add 2 << addFive *)

(* let strLen = String.length *)
(* let listLen (s: List<_>) = s.Length *)

(* let intToString x = printf "x is %i %b" x true *)

(* let add a b = a + b *)

(* let addOneToAll l = List.map (fun x -> add x 1) l *)

(* let foldTest l = List.fold (fun s n -> s + n) 0 l *)
(* let foldTest2 l = List.fold add 0 l *)

(* let sort l = List.sort l *)

(* let recTest () = *)
(*     match receive<string>() with *)
(*     | "hi" as m-> Some m *)
(*     | _ -> None *)

let listMap l =
    match List.map (fun x -> x +1) l with
    | 2 :: _ -> Some l
    | _ -> None
