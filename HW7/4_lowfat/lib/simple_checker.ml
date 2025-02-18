(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)

type type_equation = typ * typ

let rec generate_type_equations (exp : M.exp) (env : (string * typ) list) : typ * type_equation list =
  match exp with
  | M.CONST (M.N _) -> (TInt, [])
  | M.CONST (M.B _) -> (TBool, [])
  | M.CONST (M.S _) -> (TString, [])
  | M.VAR x -> 
    (try (List.assoc x env, [])
      with Not_found -> raise (M.TypeError ("unbound variable: " ^ x)))
  | M.FN (x, e) ->
    let tx = TVar (new_var ()) in
    let (te, eqs) = generate_type_equations e ((x, tx) :: env) in
    (TFun (tx, te), eqs)
  | M.APP (e1, e2) ->
    let (t1, eqs1) = generate_type_equations e1 env in
    let (t2, eqs2) = generate_type_equations e2 env in
    let t = TVar (new_var ()) in
    (t, (t1, TFun (t2, t)) :: (eqs1 @ eqs2))
  | M.LET (M.VAL (x, e1), e2) ->
    let (t1, eqs1) = generate_type_equations e1 env in
    let (t2, eqs2) = generate_type_equations e2 ((x, t1) :: env) in
    (t2, eqs1 @ eqs2)
  | M.LET (M.REC (f, x, e1), e2) ->
    let tx = TVar (new_var ()) in
    let tf = TVar (new_var ()) in
    let (t1, eqs1) = generate_type_equations e1 ((f, TFun (tx, tf)) :: (x, tx) :: env) in
    let (t2, eqs2) = generate_type_equations e2 ((f, TFun (tx, tf)) :: env) in
    (t2, (t1, tf) :: (eqs1 @ eqs2))
  | M.IF (e1, e2, e3) ->
    let (t1, eqs1) = generate_type_equations e1 env in
    let (t2, eqs2) = generate_type_equations e2 env in
    let (t3, eqs3) = generate_type_equations e3 env in
    (t2, (t1, TBool) :: (t2, t3) :: (eqs1 @ eqs2 @ eqs3))
  | M.BOP (op, e1, e2) ->
    let (t1, eqs1) = generate_type_equations e1 env in
    let (t2, eqs2) = generate_type_equations e2 env in
    (match op with
    | M.ADD | M.SUB ->
      (TInt, (t1, TInt) :: (t2, TInt) :: (eqs1 @ eqs2))
    | M.EQ ->
      (TBool, (t1, t2) :: (eqs1 @ eqs2))
    | M.AND | M.OR ->
      (TBool, (t1, TBool) :: (t2, TBool) :: (eqs1 @ eqs2))
    )  | M.READ -> (TInt, [])
  | M.WRITE e ->
    let (t, eqs) = generate_type_equations e env in
    (t, eqs)
  | M.MALLOC e ->
    let (t, eqs) = generate_type_equations e env in
    (TLoc t, eqs)
  | M.ASSIGN (e1, e2) ->
    let (t1, eqs1) = generate_type_equations e1 env in
    let (t2, eqs2) = generate_type_equations e2 env in
    (t2, (t1, TLoc t2) :: (eqs1 @ eqs2))
  | M.BANG e ->
    let (t, eqs) = generate_type_equations e env in
    let tv = TVar (new_var ()) in
    (tv, (t, TLoc tv) :: eqs)
  | M.SEQ (e1, e2) ->
    let (t1, eqs1) = generate_type_equations e1 env in
    let (t2, eqs2) = generate_type_equations e2 env in
    (t2, eqs1 @ eqs2)
  | M.PAIR (e1, e2) ->
    let (t1, eqs1) = generate_type_equations e1 env in
    let (t2, eqs2) = generate_type_equations e2 env in
    (TPair (t1, t2), eqs1 @ eqs2)
  | M.FST e ->
    let (t, eqs) = generate_type_equations e env in
    let t1 = TVar (new_var ()) in
    let t2 = TVar (new_var ()) in
    (t1, (t, TPair (t1, t2)) :: eqs)
  | M.SND e ->
    let (t, eqs) = generate_type_equations e env in
    let t1 = TVar (new_var ()) in
    let t2 = TVar (new_var ()) in
    (t2, (t, TPair (t1, t2)) :: eqs)
  
let rec unify (eqs : type_equation list) : (var * typ) list =
  match eqs with
  | [] -> []
  | (t1, t2) :: rest ->
    if t1 = t2 then unify rest
    else match (t1, t2) with
    | (TVar v, t) | (t, TVar v) ->
      if occurs v t then raise (M.TypeError "unification failure: occurs check")
      else (v, t) :: (unify (subst_eqs v t rest))
    | (TFun (t1, t2), TFun (t1', t2')) ->
      unify ((t1, t1') :: (t2, t2') :: rest)
    | (TPair (t1, t2), TPair (t1', t2')) ->
      unify ((t1, t1') :: (t2, t2') :: rest)
    | (TLoc t1, TLoc t2) ->
      unify ((t1, t2) :: rest)
    | _ -> raise (M.TypeError "unification failure")

and occurs (v : var) (t : typ) : bool =
  match t with
  | TVar v' -> v = v'
  | TFun (t1, t2) -> occurs v t1 || occurs v t2
  | TPair (t1, t2) -> occurs v t1 || occurs v t2
  | TLoc t -> occurs v t
  | _ -> false

and subst_eqs (v : var) (t : typ) (eqs : type_equation list) : type_equation list =
  List.map (fun (t1, t2) -> (subst v t t1, subst v t t2)) eqs

and subst (v : var) (t : typ) (t' : typ) : typ =
  match t' with
  | TVar v' -> if v = v' then t else t'
  | TFun (t1, t2) -> TFun (subst v t t1, subst v t t2)
  | TPair (t1, t2) -> TPair (subst v t t1, subst v t t2)
  | TLoc t1 -> TLoc (subst v t t1)
  | _ -> t'
    

let check : M.exp -> M.types = fun exp ->
  let (t, eqs) = generate_type_equations exp [] in
  let subs = unify eqs in
  let rec apply_subs subs t =
    match t with
    | TVar v -> (try List.assoc v subs with Not_found -> t)
    | TFun (t1, t2) -> TFun (apply_subs subs t1, apply_subs subs t2)
    | TPair (t1, t2) -> TPair (apply_subs subs t1, apply_subs subs t2)
    | TLoc t -> TLoc (apply_subs subs t)
    | _ -> t
  in
  let t' = apply_subs subs t in
  let rec typ_to_mtype t =
    match t with
    | TInt -> M.TyInt
    | TBool -> M.TyBool
    | TString -> M.TyString
    | TPair (t1, t2) -> M.TyPair (typ_to_mtype t1, typ_to_mtype t2)
    | TLoc t -> M.TyLoc (typ_to_mtype t)
    | TFun (t1, t2) -> M.TyArrow (typ_to_mtype t1, typ_to_mtype t2)
    | _ -> raise (M.TypeError "unexpected type")
  in
  typ_to_mtype t'
