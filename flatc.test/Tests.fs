module Tests

open System
open Xunit
open cerl
open Flat.Compiler



let mkLetTest () =
    let mkVar x = constr (cerl.Var x)
    let f = mkFunction "square_plus_one" 1
    let erlang = litAtom "erlang" |> constr
    let mul = litAtom "*" |> constr
    let plus = litAtom "+" |> constr
    let c = modCall erlang mul [mkVar "_x0"; mkVar "_x0"] |> constr
    let c2 = modCall erlang plus [mkVar "_x1"; litInt 1 |> constr] |> constr
    let l = mkLet "_x1" c c2
    let lam = lambda  ["_x0"] (constr l)
    let fd = funDef f lam
    Module (Atom "Test", [f], [], [fd])

[<Fact>]
let ``Module.prt`` () =
    let square =
        Module (Atom "test", [Function (Atom "square", 1) ],
                             [],
                             [FunDef(Constr (Function (Atom "square", 1)),
                                     Constr (Lambda(["_cor0"],
                                                      Exp (Constr (ModCall ((Exp (Constr (Lit (LAtom (Atom "erlang")))),
                                                                             Exp (Constr (Lit (LAtom (Atom "*"))))),
                                                                                [Exp (Constr (Var "_cor0"))
                                                                                 Exp (Constr (Var "_cor0"))]))))))])
    cerl.prt square |> printfn "%s"
    Assert.True(true)

[<Fact>]
let ``let test`` () =
    mkLetTest () |>  cerl.prt |> printfn "%s"


