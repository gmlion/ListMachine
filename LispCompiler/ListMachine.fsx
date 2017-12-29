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
        (env, (``program-lookup`` label prog))
    | Seq (BranchIfNil (variable, label), i) ->
        match ``var-lookup`` variable (env) with
        | Nil -> (env, (``program-lookup`` label prog))
        | Cell (_,_) -> (env, i)
    | Seq (FetchField (v1, Head, v2), i) ->
        let variable = ``var-lookup`` v1 env
        match variable with
        | Nil -> (env, i)
        | Cell (value, _) -> ((``var-set`` (v2, value) env), i)
    | Seq (FetchField (v1, Tail, v2), i) ->
        let variable = ``var-lookup`` v1 env
        match variable with
        | Nil -> (env, i)
        | Cell (_, value) -> ((``var-set`` (v2, value) env), i)
    | Seq (Cons (v0, v1, v'), i) ->
        ((``var-set`` (v', Cell (``var-lookup`` v0 env, ``var-lookup`` v1 env)) env), i)
    | _ -> failwith "Not implemented"

let debugStep callStack env instruction prog =
    do callStack := instruction::!callStack
    step env instruction prog

let ``run-prog`` prog =
    let env = Env (Map.add (Variable.Name "v0") Nil Map.empty)
    let rec eval env instruction prog =
        let (newEnv, t') = step env instruction prog
        if t' = Halt then
            env
        else eval newEnv t' prog
    eval env (``program-lookup`` (Label.Id 0) prog) prog

let ``debug-prog`` prog =
    let env = Env (Map.add (Variable.Name "v0") Nil Map.empty)
    // Debugging structures
    let callStack = ref List<Instruction>.Empty
    let envStack = ref List<Environment>.Empty
    
    let rec eval env instruction prog =
        let (newEnv, t') = debugStep callStack env instruction prog
        do envStack := env::!envStack
        if t' = Halt then
            printfn "%A" envStack
            printfn "%A" callStack
            env
        else eval newEnv t' prog
    eval env (``program-lookup`` (Label.Id 0) prog) prog

let sampleProgram =
    Prog
        [(Id 0, Cons (Name "v0", Name "v0", Name "v1") @@ Cons (Name "v0", Name "v1", Name "v1") @@ Cons (Name "v0", Name "v1", Name "v1") @@ Jump (Id 1));
        (Id 1, BranchIfNil (Name "v1", Id 2) @@ FetchField (Name "v1", Tail, Name "v1") @@ BranchIfNil (Name "v0", Id 1) @@ Jump (Id 2));
        (Id 2, Halt)]

``run-prog`` sampleProgram
``debug-prog`` sampleProgram

// ---------- types --------------

type ty = Ty_nil | Ty_list of ty | Ty_listcons of ty

let rec check_subType t1 t2 =
   match t1, t2 with
   | Ty_nil, Ty_nil -> true
   | Ty_nil, Ty_list t1 -> true
   | Ty_nil, Ty_listcons t1 -> false
   | Ty_list s1, Ty_nil -> false
   | Ty_list s1, Ty_list t1 -> check_subType s1 t1
   | Ty_list s1, Ty_listcons t1 -> false
   | Ty_listcons s1, Ty_nil -> false
   | Ty_listcons s1, Ty_list t1 -> check_subType s1 t1
   | Ty_listcons s1, Ty_listcons t1 -> check_subType s1 t1

let rec lub s t =
   match s, t with
   | Ty_nil, Ty_nil -> Ty_nil
   | Ty_nil, Ty_list t1 -> Ty_list t1
   | Ty_nil, Ty_listcons t1 -> Ty_list t1
   | Ty_list s1, Ty_nil -> Ty_list s1
   | Ty_list s1, Ty_list t1 -> Ty_list (lub s1 t1)
   | Ty_list s1, Ty_listcons t1 -> Ty_list (lub s1 t1)
   | Ty_listcons s1, Ty_nil -> Ty_list s1
   | Ty_listcons s1, Ty_list t1 -> Ty_list (lub s1 t1)
   | Ty_listcons s1, Ty_listcons t1 -> Ty_listcons (lub s1 t1)

// typing env
type tenv =
     | TEnv of Map<Variable, ty>

// program typing
type pi  =
     | PI of Map<Label,tenv>

let ``check-env-sub`` (TEnv tenv1) (TEnv tenv2) =
    let check key t =
        Map.exists (fun key element -> element = t) tenv2
    Map.forall check tenv1

let ``typecheck-branch`` (PI pi) tenv1 l =
    let findLabel = Map.tryFind l pi
    match findLabel with
    | None -> false
    | Some tenv2 -> ``check-env-sub`` tenv1 tenv2

