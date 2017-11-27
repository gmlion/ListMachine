type expression =
    | VAR of string
    | NUMBER of float
    | SUM of expression * expression
    | MULT of expression * expression

type environment = E of Map<string, expression>

let sum tuple =
    match tuple with
    | (NUMBER x, NUMBER y) ->
        NUMBER (x + y)
    | _ -> NUMBER 0.0

let mult tuple =
    match tuple with
    | (NUMBER x, NUMBER y) ->
        NUMBER (x * y)
    | _ -> NUMBER 0.0

let rec (/|) exp environment =
    match environment with
    | E env ->
        match exp with
        | VAR v -> (Map.find v env /| environment)
        | NUMBER n -> NUMBER n
        | SUM(exp1, exp2) -> (sum ((exp1 /| environment), (exp2 /| environment)))
        | MULT(exp1, exp2) -> (mult ((exp1  /| environment), (exp2 /| environment)))

let ( +|+ ) (binding: string * expression) environment =
    match environment with
    | E env ->
        (E (env.Add binding))

let MainEnv = E (Map.ofList [ ("a", NUMBER 1.0); ("b", NUMBER 2.0); ("c", NUMBER 3.0) ])

(SUM ((VAR "b"), (VAR "c"))) /| MainEnv

let NewEnv = ("k", (SUM ((VAR "b"), (VAR "c")))) +|+ MainEnv

(VAR "k") /| NewEnv