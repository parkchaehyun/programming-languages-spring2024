(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 *)

open M

type var = string

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TVarWrite of var
  | TVarCompare of var 
(* Modify, or add more if needed *)

type typ_scheme = SimpleTyp of typ | GenTyp of (var list * typ)
type typ_env = (M.id * typ_scheme) list

let count = ref 0

let new_var () =
  let _ = count := !count + 1 in
  "x_" ^ string_of_int !count

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 =
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

let sub_ftv ftv_1 ftv_2 = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> (var list * var list * var list) = function
  | TInt | TBool | TString -> ([], [], [])
  | TPair (t1, t2) -> 
      let (ftv1, write1, eq1) = ftv_of_typ t1 in
      let (ftv2, write2, eq2) = ftv_of_typ t2 in
      (union_ftv ftv1 ftv2, union_ftv write1 write2, union_ftv eq1 eq2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) -> 
      let (ftv1, write1, eq1) = ftv_of_typ t1 in
      let (ftv2, write2, eq2) = ftv_of_typ t2 in
      (union_ftv ftv1 ftv2, union_ftv write1 write2, union_ftv eq1 eq2)
  | TVar v -> ([v], [], [])
  | TVarWrite v -> ([], [v], [])
  | TVarCompare v -> ([], [], [v])

let ftv_of_scheme : typ_scheme -> (var list * var list * var list) = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> 
      let (ftv, write, eq) = ftv_of_typ t in
      (sub_ftv ftv alphas, sub_ftv write alphas, sub_ftv eq alphas)


let ftv_of_env : typ_env -> (var list * var list * var list) =
  fun tyenv ->
    List.fold_left
      (fun (acc_ftv, acc_write, acc_eq) (id, tyscm) ->
        let (ftv, write, eq) = ftv_of_scheme tyscm in
        (union_ftv acc_ftv ftv, union_ftv acc_write write, union_ftv acc_eq eq))
      ([], [], []) tyenv

let generalize : typ_env -> typ -> typ_scheme =
fun tyenv t ->
  let (env_ftv, env_write, env_eq) = ftv_of_env tyenv in
  let (typ_ftv, typ_write, typ_eq) = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  let write = sub_ftv typ_write env_write in
  let eq = sub_ftv typ_eq env_eq in
  let all_ftv = union_ftv (union_ftv ftv write) eq in
  if List.length all_ftv = 0 then SimpleTyp t else GenTyp (all_ftv, t)
       

(* Definitions related to substitution *)
type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst =
 fun x t ->
  let rec subs t' =
    match t' with
    | TVarWrite x'
    | TVarCompare x'
    | TVar x' -> if x = x' then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in
  subs

let ( @@ ) s1 s2 t = s1 (s2 t) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let (ftv, write, eq) = ftv_of_typ t in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta ->
          if List.mem alpha write then make_subst alpha (TVarWrite beta) @@ acc_subst
          else if List.mem alpha eq then make_subst alpha (TVarCompare beta) @@ acc_subst
          else make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env =
 fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

let rec isInside (v, t) = match t with
  | TVar v'
  | TVarWrite v'
  | TVarCompare v' -> v = v'
  | TFun (t1, t2)
  | TPair (t1, t2) -> isInside (v, t1) || isInside (v, t2)
  | TLoc t' -> isInside (v, t')
  | _ -> false

let rec unify: typ -> typ -> subst = fun t t' ->
  match (t, t') with
  | t1, t2 when t1 = t2 -> empty_subst
  | (TInt, TInt) | (TBool, TBool)| (TString, TString) -> empty_subst
  | (TVar x, _) -> if isInside (x, t') then raise (M.TypeError "Impossible") else make_subst x t'
  | (_, TVar y) -> if isInside (y, t) then raise (M.TypeError "Impossible") else make_subst y t
  | TPair (t1, t2), TPair (t1', t2') | TFun (t1, t2), TFun (t1', t2') ->
      let s1 = unify t1 t1' in
      let s2 = unify (s1 t2) (s1 t2') in
      s2 @@ s1
  | TLoc t1, TLoc t2 -> unify t1 t2
  | TVarCompare v', t'' | t'', TVarCompare v' ->
  if isInside (v', t'') then
      raise (M.TypeError "Type Error")
  else ( match t'' with
    | TVarWrite v'' -> if v'=v'' then empty_subst else make_subst v' t''
    | TInt | TBool | TString | TLoc _| TVarCompare _ -> make_subst v' t''
    | _ -> raise (M.TypeError "Type Error")
  )
  | TVarWrite v', t'' | t'', TVarWrite v' ->
    if isInside (v', t'') then
      raise (M.TypeError "Type Error")
    else ( match t'' with
      | TInt | TBool | TString | TVarWrite _  -> make_subst v' t''
      | _ -> raise (M.TypeError "Type Error")
    )
  | (_, _) -> raise (M.TypeError "Type")


(* Utility function to convert typ to string for better error messages *)
and string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TPair (t1, t2) -> "pair(" ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ ")"
  | TLoc t -> "loc(" ^ string_of_typ t ^ ")"
  | TFun (t1, t2) -> "fun(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | TVar v -> v
  | TVarWrite v -> v
  | TVarCompare v -> v

(* Expansive function *)
let rec expansive = function
  | M.CONST _ -> false
  | M.VAR _ -> false
  | M.FN _ -> false
  | M.LET (M.VAL (x, e1), e2) -> expansive e1 || expansive e2
  | M.LET (M.REC (_, _, _), e2) -> expansive e2
  | M.APP (e1, e2) -> true
  | M.IF (e1, e2, e3) -> expansive e1 || expansive e2 || expansive e3
  | M.MALLOC _ -> true
  | M.ASSIGN (e1, e2) -> expansive e1 || expansive e2
  | M.BANG e -> expansive e
  | M.SEQ (e1, e2) -> expansive e1 || expansive e2
  | M.PAIR (e1, e2) -> expansive e1 || expansive e2
  | M.FST e -> expansive e
  | M.SND e -> expansive e
  | M.BOP (_, e1, e2) -> expansive e1 || expansive e2
  | M.READ -> false
  | M.WRITE e -> expansive e

(* Instantiate a type scheme to a fresh type *)
let instantiate : typ_scheme -> typ =
  let rec inst (vars, t) subs =
    match t with
    | TVarCompare v
    | TVarWrite v
    | TVar v -> (try List.assoc v subs with Not_found -> t)
    | TPair (t1, t2) -> TPair (inst (vars, t1) subs, inst (vars, t2) subs)
    | TLoc t -> TLoc (inst (vars, t) subs)
    | TFun (t1, t2) -> TFun (inst (vars, t1) subs, inst (vars, t2) subs)
    | TInt | TBool | TString -> t
  in
  function
  | SimpleTyp t -> t
  | GenTyp (vars, t) ->
      let subs = List.map (fun v -> (v, TVar (new_var ()))) vars in
      inst (vars, t) subs


(* W algorithm with enhanced unification *)
let rec w (env : typ_env) (exp : M.exp) : subst * typ =
  match exp with
  | M.CONST (M.S _) -> (empty_subst, TString)
  | M.CONST (M.N _) -> (empty_subst, TInt)
  | M.CONST (M.B _) -> (empty_subst, TBool)
  | M.VAR x ->
    if (List.mem_assoc x env)
    then
     (let tysch = List.assoc x env in
       (match tysch with
        | SimpleTyp t -> (empty_subst, t)
        | GenTyp (alphas, t) ->
          let newgen = subst_scheme empty_subst (GenTyp (alphas, t)) in
         (match newgen with
          | SimpleTyp t -> raise (M.TypeError "No SimpleTyp")
          | GenTyp (betas, t') -> (empty_subst, t'))))
          else raise (M.TypeError "Unbound variable")      
  | M.FN (x, e) ->
      let tv = TVar (new_var ()) in
      let env' = (x, SimpleTyp tv) :: env in
      let (s1, t1) = w env' e in
      (s1, TFun (s1 tv, t1))
  | M.APP (e1, e2) ->
      let (s1, t1) = w env e1 in
      let (s2, t2) = w (subst_env s1 env) e2 in
      let tv = TVar (new_var ()) in
      let s3 = unify (s2 t1) (TFun (t2, tv)) in
      (s3 @@ s2 @@ s1, s3 tv)
  | M.LET (M.VAL (x, e1), e2) ->
      let (s1, t1) = w env e1 in
      let env' = subst_env s1 env in
      let ts = if expansive e1 then SimpleTyp t1 else generalize env' t1 in
      let env'' = (x, ts) :: env' in
      let (s2, t2) = w env'' e2 in
      (s2 @@ s1, t2)
  | M.LET (M.REC (f, x, e1), e2) ->
      let tv1 = TVar (new_var ()) in
      let env' = (f, SimpleTyp tv1) :: env in
      let (s1, t1) = w env' (M.FN (x, e1)) in
      let s2 = unify (s1 tv1) t1 in
      let ts = generalize (subst_env s1 env) t1 in
      let env'' = (f, ts) :: subst_env s1 env in
      let (s3, t2) = w env'' e2 in
      (s3 @@ s2 @@ s1, t2)  
  | M.IF (e1, e2, e3) ->
      let (s1, t1) = w env e1 in
      let s1' = unify t1 TBool in
      let s1'' = s1' @@ s1 in
      let (s2, t2) = w (subst_env s1'' env) e2 in
      let (s3, t3) = w (subst_env s1'' env) e3 in
      let s3' = unify t2 t3 in
      (s3' @@ s3 @@ s2 @@ s1'', s3' t2)
  | M.MALLOC e ->
      let (s, t) = w env e in
      (s, TLoc t)
  | M.ASSIGN (e1, e2) ->
      let (s1, t1) = w env e1 in
      let (s2, t2) = w (subst_env s1 env) e2 in
      let s3 = unify (s2 t1) (TLoc t2) in
      (s3 @@ s2 @@ s1, s3 t2)
  | M.BANG e ->
      let (s, t) = w env e in
      let tv = TVar (new_var ()) in
      let s' = unify (TLoc tv) t in
      (s' @@ s, s' tv)
  | M.SEQ (e1, e2) ->
      let (s1, _) = w env e1 in
      let (s2, t2) = w (subst_env s1 env) e2 in
      (s2 @@ s1, t2)
  | M.PAIR (e1, e2) ->
      let (s1, t1) = w env e1 in
      let (s2, t2) = w (subst_env s1 env) e2 in
      (s2 @@ s1, TPair (s2 t1, t2))
  | M.FST e ->
      let (s, t) = w env e in
      let tv1 = TVar (new_var ()) in
      let tv2 = TVar (new_var ()) in
      let s' = unify (TPair (tv1, tv2)) t in
      (s' @@ s, s' tv1)
  | M.SND e ->
      let (s, t) = w env e in
      let tv1 = TVar (new_var ()) in
      let tv2 = TVar (new_var ()) in
      let s' = unify (TPair (tv1, tv2)) t in
      (s' @@ s, s' tv2)
  | M.BOP (M.EQ, e1, e2) ->
    let (s1, t1) = w env e1 in
    let v = TVarCompare (new_var ()) in
    let s2 = unify t1 v in
    let env' = subst_env s2 (subst_env s1 env) in
    let (s3, t2) = w env' e2 in
    let s4 = unify (s3 t1) t2 in
    (s4 @@ s3 @@ s2 @@ s1, TBool)
  | M.BOP (M.ADD, e1, e2)
  | M.BOP (M.SUB, e1, e2) ->
      let (s1, t1) = w env e1 in
      let (s2, t2) = w (subst_env s1 env) e2 in
      let s3 = unify t1 TInt in
      let s4 = unify t2 TInt in
      (s4 @@ s3 @@ s2 @@ s1, TInt)
  | M.BOP (M.AND, e1, e2)
  | M.BOP (M.OR, e1, e2) ->
      let (s1, t1) = w env e1 in
      let (s2, t2) = w (subst_env s1 env) e2 in
      let s3 = unify t1 TBool in
      let s4 = unify t2 TBool in
      (s4 @@ s3 @@ s2 @@ s1, TBool)
  | M.READ -> (empty_subst, TInt)
  | M.WRITE e ->
    let (s, t) = w env e in
    let v = new_var () in
    let s' = unify t (TVarWrite v) in
    (s' @@ s, s' (TVarWrite v))

(* Convert our typ to M.typ *)
let rec convert_typ (t : typ) : M.typ =
  match t with
    | TInt -> M.TyInt
    | TBool -> M.TyBool
    | TString -> M.TyString
    | TPair (t1, t2) -> M.TyPair (convert_typ t1, convert_typ t2)
    | TLoc t -> M.TyLoc (convert_typ t)
    | TFun (t1, t2) -> raise (M.TypeError "Function types are not supported in M.typ")
    | _ -> raise (M.TypeError "Type")

(* Check function *)
let check (exp : M.exp) : M.typ =
  let (_, t) = w [] exp in
    convert_typ t

