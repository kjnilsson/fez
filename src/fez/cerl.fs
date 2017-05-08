module cerl


[<AutoOpen>]
module Util =
    let (|Indent|) num =
        String.replicate num " "

    let prtNl() = printf "\r\n"

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
        static member prt lit =
            match lit with
            | LChar c ->
                sprintf "$%c'" c
            | LString s ->
                sprintf "\"%s\"" s
            | LInt i ->
                sprintf "%i" i
            | LAtom (Atom atom) ->
                sprintf "'%s'" atom
            | LNil ->
                "[]"
            | x -> failwithf "%A not impl" x

type ExprList<'T> =
    | L of 'T list // no tail expr
    | LL of 'T list * 'T //head elements * tail
with
    static member prt printT l =
        match l with
        | L ls ->
            let heads = String.concat "," (List.map printT ls)
            sprintf "[%s]" heads
        | LL (ls, tail) ->
            let t = printT tail
            let heads = String.concat "," (List.map printT ls)
            sprintf "[%s|%s]" heads t

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
        | PLit l -> Literal.prt l
        | PTuple tup ->
            let pats = List.map Pat.prt tup
            sprintf "{%s}" (String.concat "," pats)
        | PList (LL(hds, tl)) ->
            let hds = List.map Pat.prt hds
            let tl = Pat.prt tl
            sprintf "[%s | %s]" (String.concat "," hds) tl
        | PAlias (Alias (v, p)) ->
            let p = Pat.prt p
            sprintf "%s = %s" v p
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
        | Pats ps ->
            ps |> List.map Pat.prt
            |> String.concat ","
            |> sprintf "<%s>"

and Alt = Alt of Pats * Guard * Exps
with
    static member prt ((Indent indent) as i) (Alt (pats, Guard guardExps, exps)) =
        let pat = Pats.prt pats
        let guard = Exps.prt 0 guardExps
        let body = Exps.prt (i+4) exps
        sprintf "%s%s when %s ->\r\n%s\r\n" indent pat guard body

and TimeOut = TimeOut of Exps * Exps
with
    static member prt ((Indent indent) as i) (TimeOut (e, b)) =
        let e = Exps.prt 0 e
        let b = Exps.prt 0 b
        sprintf "%safter %s ->\r\n%s" indent e b

and Ann<'T> =
    | Constr of 'T      // ^ core erlang construct
    | Ann of 'T * List<Const> // ^ core erlang annotated construct

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
    | Let of (Var list * Exps) * Exps      // ^ local declaration
    | LetRec of List<FunDef> * Exps       // ^ letrec expression
    | Case of Exps * List<Ann<Alt>>        // ^ @case@ /exp/ @of@ /alts/ end
    | Tuple of List<Exps>               // ^ tuple expression
    | List of ExprList<Exps>           // ^ list expression
    | Binary of List<BitString<Exps>>    // ^ binary expression
    | Op of Atom * List<Exps>             // ^ operator application
    | Try of Exps * (List<Var> * Exps) * (List<Var> * Exps) // ^ try expression
    | Receive of Ann<Alt> list * TimeOut      // ^ receive expression
    | Catch of Exps                 // ^ catch expression
    with
    static member prt ((Indent indent) as i) expr =
        match expr with
        | Var v -> sprintf "%s" v
        | Lit lit ->
            Literal.prt lit
        | Lambda (vars, exps) ->
            let expsp = Exps.prt (i+4) exps
            let varsp = String.concat "," vars
            sprintf "%sfun (%s) ->\r\n%s" indent varsp expsp
        | Fun (Function (Atom name, arity)) ->
            sprintf "'%s'/%i" name arity
        | App (targetExps, args) ->
            let target = Exps.prt (i+4) targetExps
            let argsp = args |> List.map (Exps.prt i) |> String.concat ","
            sprintf "%sapply %s (%s)" indent target argsp
        | ModCall ((left, right), args) ->
            let leftExp = Exps.prt 0 left
            let rightExp = Exps.prt 0 right
            let argsp = args |> List.map (Exps.prt 0) |> String.concat ","
            sprintf "%scall %s:%s(%s)" indent leftExp rightExp argsp
        | Let ((v, e), next) ->
            let vars = String.concat "," v
            let assign = Exps.prt 0 e
            let next' = Exps.prt 0 next
            sprintf "%slet <%s> = %s\r\n%sin %s" indent vars assign indent next'
        | Case (caseExpr, alts) ->
            let caseExpr = Exps.prt 0 caseExpr
            let alts =
                List.fold(fun s a ->
                    match a with
                    | Constr a ->
                        let x = Alt.prt (i+4) a
                        sprintf "%s%s" s x
                    | x -> failwithf "not imple %A" x) "" alts
            sprintf "%scase %s of\r\n%s%send" indent caseExpr alts indent
        | Receive (alts, after) ->
            let alts =
                List.fold(fun s a ->
                    match a with
                    | Constr a ->
                        let x = Alt.prt (i+4) a
                        sprintf "%s%s" s x
                    | x -> failwithf "not imple %A" x) "" alts
            let t = TimeOut.prt 0 after
            sprintf "%sreceive\r\n%s%s%s" indent alts indent t
         | Tuple vals ->
             List.map (Exps.prt 0) vals
             |> String.concat ","
             |> sprintf "%s{%s}" indent
        | List l ->
            let p = Exps.prt 0
            ExprList<Exps>.prt p l
        | Seq (first, second) ->
            let f = Exps.prt i first
            let s = Exps.prt i second
            sprintf "do %s\r\n%s%s" f indent s
        | x -> failwithf "Exp.prt not impl: %A" x

and Exps =
    | Exp of Ann<Exp> // single expression
    | Exps of Ann<Ann<Exp> list> // annotated list of expressions
    with
    static member empty =
        Exps (Constr [])
    static member prt ((Indent indent) as i) expr =
        match expr with
        | Exp (Constr expr) ->
            Exp.prt i expr
        | Exps (Constr expr) ->
            let exps =
                expr
                |> List.choose (function
                                | Constr e -> Some (Exp.prt 0 e)
                                | _ -> None)
            sprintf "<%s>" (String.concat "," exps)
        | x -> failwithf "Exps.prt not impl: %A" x

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
                  yield Function.prt indent f |> sprintf ",%s"
          | _ -> ()
          yield "]"
          yield ""
          yield "    attributes []" //TODO:
          for d in defs do
              yield FunDef.prt d |> sprintf "%s"
          yield "end"
        ]


let defaultGuard = Guard (Exp (Constr (Lit (LAtom (Atom "true")))))

let rec mergePat (a, b) =
    match (a,b) with
    | PList (LL (heads, tails)),
        PList (LL (heads2, tails2)) ->
        let tails = mergePat (tails, tails2)
        let heads = heads @ heads2
        PList (LL (heads, tails))
    | PList (LL (heads, tails)), (PLit LNil as nil)->
        PList (LL (heads, nil))
    | PTuple aps, PTuple bps ->
        List.zip aps bps
        |> List.map mergePat
        |> PTuple
    | PVar "_", b -> b
    | a, PVar "_" -> a
    | x -> failwithf "mergePat not impl %A" x

let constr x =
    Exp (Constr x)

let altExpr x = Constr <| Alt x

let litAtom name =
    Lit (LAtom (Atom name))

let wrap a (Guard b) =
    let alt1 = altExpr (Pats [], a, b)
    let alt2 = altExpr (Pats [], defaultGuard, litAtom "false" |> constr)
    Guard (constr <| Case (Exps (Constr []), [alt1;alt2]))

let mergeGuards (a, b) =
                if a = defaultGuard then b
                elif b = defaultGuard then a
                else wrap a b

let prt = Module.prt >> String.concat System.Environment.NewLine


let op_ComposeRight =
    FunDef
      (Constr (Function (Atom "op_ComposeRight",2)),
       (Constr
         (Lambda
            (["_f0"; "_g0"],
             (Exp
               (Constr
                 (Lambda (["_x0"],
                     (Exp
                       (Constr
                          (App
                             (Exp (Constr (Var "_g0")),
                              [Exp
                                 (Constr
                                    (App
                                       (Exp (Constr (Var "_f0")),
                                        [Exp (Constr (Var "_x0"))])))]))))))))))))
let op_ComposeLeft =
    FunDef
      (Constr (Function (Atom "op_ComposeLeft",2)),
       (Constr
         (Lambda
            (["_g0"; "_f0"],
             (Exp
               (Constr
                 (Lambda (["_x0"],
                     (Exp
                       (Constr
                          (App
                             (Exp (Constr (Var "_g0")),
                              [Exp
                                 (Constr
                                    (App
                                       (Exp (Constr (Var "_f0")),
                                        [Exp (Constr (Var "_x0"))])))]))))))))))))
