type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key

type map = End of treasure (* term *)
        | Branch of map * map (*func app*)
        | Guide of string * map (*func def*)

type varType = VarBar
          | Variable of string
          | Function of (varType * varType)

exception IMPOSSIBLE

let counter = ref 0

let varList = ref []

let getUniqueVar () =
  counter := !counter + 1;
  Variable ("unique_" ^ string_of_int !counter)

let rec apply (subst, term) = match term with
  | VarBar -> VarBar
  | Variable v -> 
      (try
         let _, replacement = List.find (fun (var, _) -> var = v) subst in
         replacement
       with Not_found -> term) 
  | Function (e1, e2) ->
      Function (apply (subst, e1), apply (subst, e2))

let applyEnv (subst, env) =
  List.map (fun (var, ty) -> (var, apply (subst, ty))) env

let rec isInside (v, t) = match t with
  | VarBar -> false
  | Variable v1 -> v = v1
  | Function (t1, t2) -> isInside (v, t1) || isInside (v, t2)

let rec unify (t1, t2) = match (t1, t2) with
  | (t1, t2) when t1 = t2 -> []
  | (Variable v, e) -> if (isInside (v, e)) then raise IMPOSSIBLE else [(v, e)]
  | (e, Variable v) -> if (isInside (v, e)) then raise IMPOSSIBLE else [(v, e)]
  | (Function(t1', t2'), Function(t3, t4)) ->
    let s = unify (t1', t3) in
    let s' = unify ((apply (s, t2')), (apply (s, t4))) in
    s @ s'
  | _ -> raise IMPOSSIBLE

  
let getReady map = 
  let env = ref [] in

  let rec m (map, term) = match map with
  | End treasure -> (match treasure with
    | StarBox -> let list = unify (VarBar, term) in
    varList:= !varList @ (List.map fst list);
    list
    | NameBox x -> 
    (try
      let t = (List.assoc x !env) in
      let list = unify(t, term) in
      varList:= !varList @ (List.map fst list);
      list
      
    with Not_found -> 
      let newVar = getUniqueVar() in
      env := (x, newVar) :: !env;
      let list = unify (newVar, term) in
      varList:= !varList @ (List.map fst list);
      list
    )
  )
  | Branch (m1, m2) ->
    let newVar = getUniqueVar() in
    let s = m(m1, Function(newVar, term)) in
    env := applyEnv (s, !env);
    let s' = m(m2, (apply (s, newVar))) in
    s' @ s
  | Guide (x, m1) -> 
    let newVar1 = getUniqueVar() in
    let newVar2 = getUniqueVar() in
    let s = unify(Function(newVar1, newVar2), term) in
    env := applyEnv (s, !env);
    env := !env @ [(x, apply (s, newVar1))];
    let s' = m(m1, (apply (s, newVar2))) in
    s' @ s
  in
  let list = m(map, getUniqueVar()) in
  let rec convertToKey t = match t with
    | VarBar -> Bar
    | Variable x -> (try
    let k = (List.assoc x list) in
    convertToKey(k)
    with Not_found -> 
      Bar
    ) 
    | Function (t1, t2) -> Node (convertToKey(t1), convertToKey(t2))
  in
  let variableList = List.map (fun x -> Variable x) !varList in
  let keys = List.map convertToKey variableList in
  List.sort_uniq compare keys