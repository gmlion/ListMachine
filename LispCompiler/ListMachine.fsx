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

exception FetchException

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
    | Seq (FetchField (v1, position, v2), i) ->
        match position with
        | Head -> 
            let variable = ``var-lookup`` v1 env
            match variable with
            | Nil -> raise FetchException
            | Cell (value, _) -> ((``var-set`` (v2, value) env), i)
        | Tail ->
            let variable = ``var-lookup`` v1 env
            match variable with
            | Nil -> raise FetchException
            | Cell (_, value) -> ((``var-set`` (v2, value) env), i)
    | Seq (Cons (v0, v1, v'), i) ->
        ((``var-set`` (v', Cell (``var-lookup`` v0 env, ``var-lookup`` v1 env)) env), i)
    | _ -> failwith "Not implemented"

let debugStep callStack env instruction prog =
    (instruction::callStack, (step env instruction prog))

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
    //let callStack = new System.Collections.Generic.List<Instruction>()
    //let envStack = new System.Collections.Generic.List<Environment>()

    let callStack = []
    let envStack = []
    
    let rec eval (callStack, envStack, env) instruction prog =
        let (callStack, (newEnv, t')) = debugStep callStack env instruction prog
        if t' = Halt then
            printfn "%A" envStack
            printfn "%A" callStack
            (env::envStack, env)
        else eval (callStack, env::envStack, newEnv) t' prog
    let (_, finalEnv) = eval (callStack, envStack, env) (``program-lookup`` (Label.Id 0) prog) prog
    finalEnv

let sampleProgram =
    Prog
        [(Id 0, Cons (Name "v0", Name "v0", Name "v1") @@ Cons (Name "v0", Name "v1", Name "v1") @@ Cons (Name "v0", Name "v1", Name "v1") @@ Jump (Id 1));
        (Id 1, BranchIfNil (Name "v1", Id 2) @@ FetchField (Name "v1", Tail, Name "v1") @@ BranchIfNil (Name "v0", Id 1) @@ Jump (Id 2));
        (Id 2, Halt)]

// ---------- test ---------------

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
        Map.exists (fun key element -> check_subType element t) tenv2
    Map.forall check tenv1

let ``typecheck-branch`` (PI pi) tenv1 l =
    let findLabel = Map.tryFind l pi
    match findLabel with
    | None -> false
    | Some tenv2 -> ``check-env-sub`` tenv1 tenv2

let rec ``typecheck-instr`` pi tenv1 i =
    match i with
    | Seq (i1, i2) ->
        let check = ``typecheck-instr`` pi tenv1 i1
        match check with
        | Some tenv2 -> ``typecheck-instr`` pi tenv2 i2
        | None -> None
    | Jump l -> None
    | BranchIfNil (v, l) ->
        match tenv1 with
        | TEnv t1 ->
            let findLabel = Map.tryFind v t1
            match findLabel with
            | None -> None
            | Some (Ty_list t) ->
                if ``typecheck-branch`` pi (TEnv (Map.add v Ty_nil t1)) l
                then Some (TEnv (Map.add v (Ty_listcons t) t1))
                else None
            | Some (Ty_listcons t) ->
                Some tenv1
            | Some Ty_nil ->
                if ``typecheck-branch`` pi tenv1 l
                then Some tenv1
                else None
    | FetchField (v1, p, v2) ->
        match tenv1 with
        | TEnv t1 ->
            let findVar = Map.tryFind v1 t1
            match findVar with
            | Some (Ty_listcons t) -> 
                match p with
                | Head -> Some (TEnv (Map.add v2 t t1))
                | Tail -> Some (TEnv (Map.add v2 (Ty_list t) t1))
            | _ -> None
    | Cons (v1, v2, v3) ->
        match tenv1 with
        | TEnv te ->
            let v1_ty = Map.tryFind v1 te
            let v2_ty = Map.tryFind v2 te
            match (v1_ty, v2_ty) with
            | (None, None)
            | (None, _)
            | (_, None) -> None
            | (Some t0, Some t1) ->
                let t = lub (Ty_list t0) t1
                match t with
                | Ty_list t' -> Some (TEnv (Map.add v3 t' te))
                | _ -> None
    | Halt -> None

let rec ``typecheck-block`` pi tenv1 i =
    match i with
    | Halt -> true
    | Seq (i1, i2) ->
        let instrCheck = ``typecheck-instr`` pi tenv1 i1
        match instrCheck with
        | Some tenv1 -> ``typecheck-block`` pi tenv1 i2
        | None -> false
    | Jump l -> ``typecheck-branch`` pi tenv1 l
    | _ -> false
       
let rec ``typecheck-blocks`` pi prog =
    match pi, prog with
    | (PI pi1, Prog prog1) ->
        match prog1 with
        | (l, i)::rest ->
            match Map.tryFind l pi1 with
            | Some tenv -> ``typecheck-block`` pi tenv i && ``typecheck-blocks`` pi (Prog rest)
            | None -> false
        | [] -> true

let ``typecheck-program`` pi prog =
    ``typecheck-blocks`` pi prog

// ------------ test ------------

let sampleTyping =
    PI (Map.empty
            .Add(Id 0, (TEnv ([ Name "v0", Ty_nil ] |> Map.ofList)))
            .Add(Id 1, (TEnv ([ Name "v0", Ty_nil; Name "v1", Ty_list Ty_nil] |> Map.ofList)))
            .Add(Id 2, (TEnv ([] |> Map.ofList))))

``typecheck-program`` sampleTyping sampleProgram