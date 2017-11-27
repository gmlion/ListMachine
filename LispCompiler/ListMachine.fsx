type Value =
    | Nil
    | Cell of Value * Value

type Label =
    | Id of int

type Variable =
    | Name of string

type Environment =
    | Env of Map<Variable, Value>

type FetchPosition =
    | Head
    | Tail

type Instruction =
    | Jump of Label
    | BranchIfNil of Variable * Label
    | FetchField of Variable * FetchPosition * Variable
    | Cons of Variable * Variable * Variable
    | Seq of Instruction * Instruction
    | Halt

let (@@) i1 i2 = Seq (i1, i2)

type Program =
    | Prog of List<Label * Instruction>

// end = empty list

let ``var-lookup`` (var: Variable) (Env env) : Value =
    Map.find var env

let ``var-set`` ((var, value): Variable * Value) (Env env) : Environment =
    Env (Map.add var value env)

let ``program-lookup`` (label: Label) (Prog prog) =
    let findLabel (l, _) =
        l = label
    let (_, result) = List.find findLabel prog
    result

let step env instruction prog =
    match instruction with
    | Jump label -> 
        printfn "Jump"
        printfn "%A" env
        (env, (``program-lookup`` label prog))
    | Seq (BranchIfNil (variable, label), i) ->
        printfn "BranchIfNil"
        printfn "%A" env
        match ``var-lookup`` variable (env) with
        | Nil -> (env, (``program-lookup`` label prog))
        | Cell (_,_) -> (env, i)
    | Seq (FetchField (v1, Head, v2), i) ->
        printfn "FetchField"
        printfn "%A" env
        let variable = ``var-lookup`` v1 env
        match variable with
        | Nil -> (env, i)
        | Cell (value, _) -> ((``var-set`` (v2, value) env), i)
    | Seq (FetchField (v1, Tail, v2), i) ->
        printfn "FetchField"
        printfn "%A" env
        let variable = ``var-lookup`` v1 env
        match variable with
        | Nil -> (env, i)
        | Cell (_, value) -> ((``var-set`` (v2, value) env), i)
    | Seq (Cons (v0, v1, v'), i) ->
        printfn "Cons"
        printfn "%A" env
        ((``var-set`` (v', Cell (``var-lookup`` v0 env, ``var-lookup`` v1 env)) env), i)
    | Halt ->
        printfn "%A" env
        failwith "Halted"
    | _ -> failwith "Not implemented"

let ``run-prog`` prog : Instruction =
    let env = Env (Map.add (Variable.Name "v0") Nil Map.empty)
    let rec eval env instruction prog =
        let (newEnv, t') = step env instruction prog
        eval newEnv t' prog
    eval env (``program-lookup`` (Label.Id 0) prog) prog

let sampleProgram =
    Prog
        [(Id 0, Cons (Name "v0", Name "v0", Name "v1") @@ Cons (Name "v0", Name "v1", Name "v1") @@ Cons (Name "v0", Name "v1", Name "v1") @@ Jump (Id 1));
        (Id 1, BranchIfNil (Name "v1", Id 2) @@ FetchField (Name "v1", Tail, Name "v1") @@ BranchIfNil (Name "v0", Id 1) @@ Jump (Id 2));
        (Id 2, Halt)]

``run-prog`` sampleProgram


