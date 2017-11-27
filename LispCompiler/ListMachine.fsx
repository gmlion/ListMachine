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

let (@@) = Seq

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

let rec step env instruction prog =
    match instruction with
    | Jump label -> step env (``program-lookup`` label prog) prog
    | Seq (BranchIfNil (variable, label), i) ->
        match ``var-lookup`` variable (env) with
        | Nil -> step env (``program-lookup`` label prog) prog
        | Cell (_,_) -> step env i prog
    | Seq (FetchField (v1, Head, v2), i) -> 
        let variable = ``var-lookup`` v1 env
        match variable with
        | Nil -> step env i prog
        | Cell (value, _) -> step (``var-set`` (v2, value) env) i prog
    | Seq (FetchField (v1, Tail, v2), i) -> 
        let variable = ``var-lookup`` v1 env
        match variable with
        | Nil -> step env i prog
        | Cell (_, value) -> step (``var-set`` (v2, value) env) i prog
    | Seq (Cons (v0, v1, v'), i) ->
        step (``var-set`` (v', Cell (``var-lookup`` v0 env, ``var-lookup`` v1 env)) env) i prog
    | Halt -> (env, Halt)
    | _ -> failwith "Not implemented"

let ``run-prog`` prog =
    let env = Env (Map.add (Variable.Name "v0") Nil Map.empty)
    step env (``program-lookup`` (Label.Id 0) prog) prog

let test = (@@) (Cons (Variable.Name "v0", Variable.Name "v0", Variable.Name "v1"), Cons (Variable.Name "v0", Variable.Name "v0", Variable.Name "v1"))

let sampleProgram =
    [(0, ((@@) ((@@) ((@@) (Cons (Variable.Name "v0", Variable.Name "v0", Variable.Name "v1"), Cons (Name "v0", Name "v1", Name "v1"))), Cons (Name "v0", Name "v1", Name "v1")), Jump (Id 1)))]


