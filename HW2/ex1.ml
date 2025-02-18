exception InvalidArgument

type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let rec diff (exp, x) = match exp with
  | CONST _ -> CONST 0
  | VAR v ->
    if v = x then CONST 1
    else CONST 0
  | POWER (v, m) ->
    if v = x then TIMES([CONST m; POWER(v, m-1)])
    else CONST 0
  | TIMES l -> (match l with
    | [] -> raise InvalidArgument
    | [e] -> diff (e, x)
    | e :: tail -> SUM [TIMES (diff (e, x) :: tail); TIMES [e; diff ((TIMES tail), x)]]
    )
  | SUM l -> ( match l with
    | [] -> raise InvalidArgument
    | _ -> SUM (List.map (fun e -> diff (e, x)) l)
    )