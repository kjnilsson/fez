module cerl


[<AutoOpen>]
module Util =
    let (|Indent|) num =
        String.replicate num " "

    let prtNl() = printf "\r\n"

type Prt =
    | Prt of (string * Prt) list
    | Term
    | Closing of string

let t : Prt =
    Prt ["module 'test' ['square'/1,", Prt ["'module_info'/0", Term
                                            "'module_info'/1", Term]
         "'square'/1", Prt ["fun (_core0) ->",
                             Prt ["call 'erlang':'*'",
                                   Prt ["(_core0, core0)", Term]]]
         "end", Term]

let rec printItem ((Indent ind) as i) (l, p) =
    printf "%s%s" ind l
    rend (i+4) p
and rend i (prt : Prt) =
    match prt with
    | Prt items ->
        for itm in items do
            printf "%s" System.Environment.NewLine
            printItem i itm
    | Closing s -> printf "%s" s
    | Term -> ()


type Var = string

type Atom = Atom of string

// | /literal/.
// Values of this type hold the abstract value of the literal, not the
// precise string representation used. For example, @10@, @0o12@ and @0xa@
// have the same representation.
type Literal =
    | LChar of char     // ^ character literal
    | LString of string   // ^ string literal
    | LInt of int  // ^ integer literal
    | LFloat of float   // ^ floating point literal
    | LAtom of Atom      // ^ atom literal
    | LNil              // ^ empty list
    with
        static member prt (Indent indent) lit =
            match lit with
            | LChar c ->
                sprintf "%s$%c'" indent c
            | LString s ->
                sprintf "%s\"%s\"'" indent s
            | LInt i ->
                sprintf "%s%i" indent i
            | LAtom (Atom atom) ->
                sprintf "%s'%s'" indent atom
            | x -> failwithf "%A not impl" x

type ExprList<'T> =
    | L of 'T
    | LL of List<'T> * 'T

type Const =
    | CLit of Literal
    | CTuple of List<Const>
    | CList of ExprList<Const>

type BitString<'T> = BitString of 'T * List<Exps>

and Pat =
    | PVar of Var                 // ^ variable
    | PLit of Literal             // ^ literal constant
    | PTuple of List<Pat>             // ^ tuple pattern
    | PList of ExprList<Pat>         // ^ list pattern
    | PBinary of List<BitString<Pat>>  // ^ list of bitstring patterns
    | PAlias of Alias             // ^ alias pattern
with
    static member prt pat =
        match pat with
        | PVar v -> v
        | PLit l -> Literal.prt 0 l
        | PTuple tup ->
            let pats = List.map Pat.prt tup
            sprintf "{%s}" (String.concat "," pats)
        | x -> failwithf "Pat.prt not impl %A" x

and Alias = Alias of Var * Pat

and Guard = Guard of Exps

and Pats =
    | Pat of Pat    // ^ single pattern
    | Pats of List<Pat> // ^ list of patterns
with
    static member prt pats =
        match pats with
        | Pat p -> sprintf "<%s>" (Pat.prt p)
        | pl -> failwithf "not impl %A" pl

and Alt = Alt of Pats * Guard * Exps
with
    static member prt ((Indent indent) as i) (Alt (pats, Guard guardExps, exps)) =
        let pat = Pats.prt pats
        let guard = Exps.prt 0 guardExps
        let body = Exps.prt (i+4) exps
        sprintf "%s%s when %s ->\r\n%s\r\n" indent pat guard body

and TimeOut = TimeOut of Exps * Exps

and Ann<'T> =
    | Constr of 'T      // ^ core erlang construct
    | Ann of 'T * List<Const> // ^ core erlang annotated construct
(* with *)
(*     static member prt indent a = *)
(*         match a with *)
(*         | Constr t -> *)
(*             (^T : (static member prt : ^T -> int -> string) indent t) *)
(*         | _ -> failwith "not imp;" *)
            (* sprintf "%s'%s'/%i" indent name arity *)

and Function = Function of Atom * int
with
    static member prt ((Indent indent) as i)
                      (Function (Atom name, arity)) =
        sprintf "%s'%s'/%i" indent name arity

// | CoreErlang expression.
and Exp =
    | Var of Var                    // ^ variable
    | Lit of Literal                // ^ literal constant
    | Fun of Function               // ^ function name
    | App of Exps * List<Exps>            // ^ application
    | ModCall of (Exps * Exps) * List<Exps> // ^ module call
    | Lambda of List<Var> * Exps          // ^ lambda expression
    | Seq of Exps * Exps              // ^ sequencing
    | Let of (List<Var> * Exps) * Exps      // ^ local declaration
    | LetRec of List<FunDef> * Exps       // ^ letrec expression
    | Case of Exps * List<Ann<Alt>>        // ^ @case@ /exp/ @of@ /alts/ end
    | Tuple of List<Exps>               // ^ tuple expression
    | List of ExprList<Exps>           // ^ list expression
    | Binary of List<BitString<Exps>>    // ^ binary expression
    | Op of Atom * List<Exps>             // ^ operator application
    | Try of Exps * (List<Var> * Exps) * (List<Var> * Exps) // ^ try expression
    | Rec of List<Ann<Alt>> * TimeOut      // ^ receive expression
    | Catch of Exps                 // ^ catch expression
    with
    static member prt ((Indent indent) as i) expr =
        match expr with
        | Var v -> sprintf "%s%s" indent v
        | Lit lit ->
            Literal.prt i lit
        | Lambda (vars, exps) ->
            let expsp = Exps.prt (i+4) exps
            let varsp = String.concat "," vars
            sprintf "%sfun (%s) ->\r\n%s" indent varsp expsp
        | App (targetExps, args) ->
            let target = Exps.prt (i+4) targetExps
            let arity = List.length args
            let argsp = args |> List.map (Exps.prt i) |> String.concat ","
            sprintf "%sapply\r\n%s/%i\r\n%s    (%s)" indent target arity indent argsp
        | ModCall ((left, right), args) ->
            let leftExp = Exps.prt (i+4) left
            let rightExp = Exps.prt 0 right
            let argsp = args |> List.map (Exps.prt 0) |> String.concat ","
            sprintf "%scall\r\n%s:%s\r\n%s    (%s)" indent leftExp rightExp indent argsp
        | Let ((v, e), next) ->
            let vars = String.concat "," v
            let assign = Exps.prt (i+4) e
            let next' = Exps.prt (i+4) next
            sprintf "%slet <%s> =\r\n%s\r\n%sin\r\n%s" indent vars assign indent next'
        | Case (caseExpr, alts) ->
            let caseExpr = Exps.prt 0 caseExpr
            let alts =
                List.fold(fun s a ->
                    match a with
                    | Constr a ->
                        let x = Alt.prt (i+4) a
                        sprintf "%s%s\r\n" s x
                    | x -> failwithf "not imple %A" x) "" alts
            sprintf "%scase %s of\r\n%send" indent caseExpr alts

        | x -> failwithf "%A not implemented" x

and Exps =
    | Exp of Ann<Exp>        // ^ single expression
    | Exps of Ann<List<Ann<Exp>>> // ^ list of expressions
    with
    static member prt ((Indent indent) as i) expr =
        match expr with
        | Exp (Constr expr) ->
            Exp.prt i expr
        | _ -> failwith "not impl"

and FunDef = FunDef of Ann<Function> * Ann<Exp>
    with
    static member prt (FunDef (def, expr)) =
        match (def, expr) with
        | (Constr f, Constr e) ->
            let fp = Function.prt 0 f
            let ep = Exp.prt 4 e
            sprintf "%s =\r\n%s\r\n" fp ep
        | _ -> failwith "Ann not implemented"

and Module = Module of Atom * List<Function> * List<Atom * Const> * List<FunDef>
    with
    static member prt (Module (Atom name, funs, attribs, defs)) =
        [ let indent = 11 + name.Length
          match funs with
          | f :: funs ->
              yield Function.prt 0 f |> sprintf "module '%s' [%s" name
              for f in funs do
                  yield Function.prt indent f |> sprintf "%s"
          | _ -> ()
          yield "]"
          yield ""
          yield "    attributes []" //TODO
          for d in defs do
              yield FunDef.prt d |> sprintf "%s"
          yield "end"
        ]



let prt = Module.prt >> String.concat System.Environment.NewLine
