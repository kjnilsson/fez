module cerl


[<AutoOpen>]
module Util =
    let (|Indent|) num =
        String.replicate num " "

    let nl = System.Environment.NewLine
    let prtNl() = printf "%s" nl


type Var = string

type Atom = Atom of string

// | /literal/.
// Values of this type hold the abstract value of the literal, not the
// precise string representation used. For example, @10@, @0o12@ and @0xa@
// have the same representation.
type Literal =
    | LChar of char     // ^ character literal
    | LString of string   // ^ string literal
    | LInt of int64  // ^ integer literal
    | LFloat of float   // ^ floating point literal
    | LAtom of Atom      // ^ atom literal
    | LNil              // ^ empty list
    with
        static member prt lit =
            match lit with
            | LChar c ->
                sprintf "$%c" c
            | LString s ->
                sprintf "\"%s\"" s
            | LInt i ->
                sprintf "%i" i
            | LFloat f ->
                sprintf "%f" f
            | LAtom (Atom atom) ->
                sprintf "'%s'" atom
            | LNil ->
                "[]"

type ExprList<'T> =
    | L of 'T list // no tail expr
    | LL of 'T list * 'T //head elements * tail
with
    static member prt (printT: 'T -> string) (l: ExprList<'T>) =
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
with
    static member fromByte (b: byte) =
        let lit l =
            Exp (Constr (Lit l))
        let litInt i =
            lit (LInt i)
        let litAtom name =
            lit (LAtom (Atom name))
        let lst xs =
            Exp (Constr (List(L xs)))
        BitString (litInt (int64 b),
                   [litInt 8L
                    litInt 1L
                    litAtom "integer"
                    lst [litAtom "unsigned"; litAtom "big"]])

    static member prt (printT: 'T -> string) (BitString (t: 'T, exps)) =
        let args = String.concat "," (List.map (Exps.prt 0) exps)
        let b = printT t
        sprintf "#<%s>(%s)" b args

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
        | PList (L(xs)) ->
            let xs = List.map (Pat.prt) xs
            sprintf "[%s]" (String.concat "," xs)
        | PAlias (Alias (v, p)) ->
            let p = Pat.prt p
            sprintf "%s = %s" v p
        | x -> failwithf "Pat.prt not impl %A" x

and Alias = Alias of Var * Pat

and Guard = Guard of Exps

and Pats =
    | Pat of Pat    // ^ single pattern
    | Pats of Pat list // ^ list of patterns
with
    static member prt pats =
        match pats with
        | Pat p -> sprintf "<%s>" (Pat.prt p)
        | Pats ps ->
            ps |> List.map Pat.prt
            |> String.concat ","
            |> sprintf "<%s>"

and Ann<'T> =
    | Constr of 'T      // ^ core erlang construct
    | Ann of 'T * List<Const> // ^ core erlang annotated construct
    static member prt (printT: 'T -> string) ann : string =
        match ann with
        | Constr t -> printT t
        | Ann (t, _) -> printT t

and Function = Function of Atom * int
with
    static member prt (Indent indent as i)
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
    | Binary of BitString<Exps> list    // ^ binary expression
    | Op of Atom * List<Exps>             // ^ operator application
    | Try of Exps * (List<Var> * Exps) * (List<Var> * Exps) // ^ try expression
    | Receive of Ann<Alt> list * TimeOut      // ^ receive expression
    | Catch of Exps                 // ^ catch expression
    | Noop of Exps                 //  not part of model - just for 
    with
    static member prt ((Indent indent) as i) expr =
        let i4 = i+4
        let commaNl = sprintf ",%s" nl
        match expr with
        | Var v -> sprintf "%s%s" indent v
        | Lit lit ->
            Literal.prt lit |> sprintf "%s%s" indent
        | Noop exps ->
            Exps.prt i exps
        | Lambda (vars, exps) ->
            let expsp = Exps.prt (i+4) exps
            let varsp = String.concat "," vars
            sprintf "%sfun (%s) ->%s%s" indent varsp nl expsp
        | Fun (Function (Atom name, arity)) ->
            sprintf "'%s'/%i" name arity
        | App (targetExps, args) ->
            let target = Exps.prt 0 targetExps
            (* let argsp = args |> List.map (Exps.prt 0) |> String.concat "," *)
            let argsp = args |> List.map (Exps.prt i4) |> String.concat commaNl
            let nl =
                match args with
                | [] -> ""
                | _ -> nl
            sprintf "%sapply %s (%s%s)" indent target nl argsp
        | Op (Atom op, args) ->
            (* let target = Exps.prt 0 op *)
            (* let argsp = args |> List.map (Exps.prt 0) |> String.concat "," *)
            let argsp = args |> List.map (Exps.prt i4) |> String.concat commaNl
            let nl =
                match args with
                | [] -> ""
                | _ -> nl
            sprintf "%sprimop '%s' (%s%s)" indent op nl argsp
        | ModCall ((left, right), args) ->
            let leftExp = Exps.prt 0 left
            let rightExp = Exps.prt 0 right
            let argsp = args |> List.map (Exps.prt i4) |> String.concat commaNl
            let nl =
                match args with
                | [] -> ""
                | _ -> nl
            sprintf "%scall %s:%s(%s%s)" indent leftExp rightExp nl argsp
        | Let ((v, e), next) ->
            let vars = String.concat "," v
            let assign = Exps.prt i4 e
            let next' = Exps.prt i4 next
            sprintf "%slet <%s> =%s%s%s%sin%s%s" indent vars nl assign nl indent nl next'
        | Case (caseExpr, alts) ->
            let caseExpr = Exps.prt i4 caseExpr
            let alts =
                alts
                |> List.fold(fun s a ->
                    match a with
                    | Constr a ->
                        let x = Alt.prt (i+4, a)
                        sprintf "%s%s" s x
                    | Ann (a, _) ->
                        let x = Alt.prt (i+4, a)
                        sprintf "%s%s" s x) ""
                        (* failwithf "not imple %A" x) "" alts *)
            sprintf "%scase%s%s of%s%s%send" indent nl caseExpr nl alts indent
        | Receive (alts, after) ->
            let alts =
                List.fold(fun s a ->
                    match a with
                    | Constr a ->
                        let x = Alt.prt (i+4, a)
                        sprintf "%s%s" s x
                    | x -> failwithf "not imple %A" x) "" alts
            let t = TimeOut.prt i after
            sprintf "%sreceive%s%s%s" indent nl alts t
         | Tuple vals ->
             let ts =
                 List.map (Exps.prt (i+1)) vals
                 |> String.concat commaNl
             sprintf "%s{%s}" indent (ts.TrimStart())
        | List l ->
            let p = Exps.prt 0
            ExprList<Exps>.prt p l
        | Binary bs ->
            let x =
                bs
                |> List.map (BitString.prt (Exps.prt 0))
                |> String.concat commaNl
            sprintf "%s#{%s}#" indent x
        | Seq (first, second) ->
            let i4 = i+4
            let f = Exps.prt i4 first
            let s = Exps.prt i4 second
            sprintf "%sdo%s%s%s%s" indent nl f nl s
        | LetRec (funs, e) ->
            let f = funs |> List.map (fun f -> FunDef.prt (i4, f)) |> String.concat nl
            let e = Exps.prt i4 e
            sprintf "%sletrec%s%s%sin%s%s" indent nl f indent nl e
        | Catch e ->
            let e = Exps.prt i4 e
            sprintf "%scatch%s%s" indent nl e
        | Try (e, (sucvals, suce), (catchvals, catche)) ->
            let vars = String.concat "," sucvals
            let e = Exps.prt i4 e
            let suce = Exps.prt i4 suce
            let cvars = String.concat "," catchvals
            let ce = Exps.prt i4 catche
            sprintf "%stry%s%s%s%sof <%s> ->%s%s%s%scatch <%s> ->%s%s" indent nl e nl indent vars nl suce nl indent cvars nl ce

and Exps =
    | Exp of Ann<Exp> // single expression
    | Exps of Ann<Ann<Exp> list> // annotated list of expressions
    with

    (* static member fromVar (v : Var) = *)
    (*     Exps (Constr (Var v)) *)
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
                                | Constr e -> Some (Exp.prt (i+4) e)
                                | _ -> None)
            let commaNl = "," + nl
            sprintf "%s<%s%s>" indent nl (String.concat commaNl exps)
        | Exps (Ann (exps, anns)) ->
            let exps = exps |> List.map (Ann.prt (Exp.prt (i+1)))
            let commaNl = "," + nl
            sprintf "%s<%s%s>" indent nl (String.concat commaNl exps)
        | x -> failwithf "Exps.prt not impl: %A" x

and TimeOut = TimeOut of Exps * Exps
with
    static member prt ((Indent indent) as i) (TimeOut (e, b)) =
        let i4 = i+4
        let e = Exps.prt 0 e
        let b = Exps.prt i4 b
        sprintf "%safter %s ->%s%s%s" indent e nl indent b

and FunDef = FunDef of Ann<Function> * Ann<Exp>
    with
    static member prt (Indent indent as i, FunDef (def, expr)) =
        match (def, expr) with
        | (Constr f, Constr e) ->
            let i4 = i+4
            let fp = Function.prt i f
            let ep = Exp.prt i4 e
            sprintf "%s =%s%s%s" fp nl ep nl
        | _ -> failwith "Ann not implemented"

and Alt = Alt of Pats * Guard * Exps
with
    static member prt (((Indent indent) as i),
                       (Alt (pats, Guard guardExps, exps))) =
        let pat = Pats.prt pats
        let guard = Exps.prt 0 guardExps
        let body = Exps.prt (i+4) exps
        sprintf "%s%s when %s ->%s%s%s" indent pat guard nl body nl

type Module = Module of Atom * List<Function> * List<Atom * Const> * List<FunDef>
    with
    static member prt (Module (Atom name, funs, attribs, defs)) =
        [ let indent = 11 + name.Length
          yield sprintf "module '%s' [" name
          match funs with
          | f :: funs ->
              yield Function.prt 0 f
              for f in funs do
                  yield Function.prt indent f |> sprintf ",%s"
          | _ -> ()
          yield "]"
          yield ""
          yield "    attributes []" //TODO:
          for d in defs do
              yield FunDef.prt (0, d) |> sprintf "%s"
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

let matchFail x : Exp =
    (Op (Atom "match_fail", x))

let wrap a (Guard b) =
    let alt1 = altExpr (Pats [], a, b)
    let alt2 = altExpr (Pats [], defaultGuard, litAtom "false" |> constr)
    Guard (constr <| Case (Exps (Constr []), [alt1;alt2]))

let mergeGuards (a, b) =
                if a = defaultGuard then b
                elif b = defaultGuard then a
                else wrap a b

let prt = Module.prt >> String.concat System.Environment.NewLine


let moduleInfo1 m =
    FunDef
      (Constr (Function (Atom "module_info", 1)),
       (Constr
         (Lambda
            (["_fez0"],
              (Exp
                (Constr
                  (ModCall
                       (((Exp (Constr (Lit (LAtom (Atom("erlang")))))),
                         (Exp (Constr (Lit (LAtom (Atom("get_module_info"))))))),
                        [(Exp (Constr (Lit (LAtom (Atom m)))))
                         (Exp (Constr (Var "_fez0")))]))))))))

let moduleInfo0 m =
    FunDef
      (Constr (Function (Atom "module_info", 0)),
       (Constr
         (Lambda
            ([],
              (Exp
                (Constr
                  (ModCall
                       (((Exp (Constr (Lit (LAtom (Atom("erlang")))))),
                         (Exp (Constr (Lit (LAtom (Atom("get_module_info"))))))),
                        [(Exp (Constr (Lit (LAtom (Atom m)))))]))))))))

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
