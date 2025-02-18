(*
 * SNU 4190.310 Programming Languages 2024 Spring
 *  K- Interpreter
 *)

(** Location Signature *)
module type LOC = sig
  type t

  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC = struct
  type t = Location of int

  let base = Location 0
  let equal (Location a) (Location b) = a = b
  let diff (Location a) (Location b) = a - b
  let increase (Location base) n = Location (base + n)
end

(** Memory Signature *)
module type MEM = sig
  type 'a t

  exception Not_allocated
  exception Not_initialized

  val empty : 'a t
  (** get empty memory *)

  val load : 'a t -> Loc.t -> 'a
  (** load value : Mem.load mem loc => value *)

  val store : 'a t -> Loc.t -> 'a -> 'a t
  (** save value : Mem.store mem loc value => mem' *)

  val alloc : 'a t -> Loc.t * 'a t
  (** get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(** Environment Signature *)
module type ENV = sig
  type ('a, 'b) t

  exception Not_bound

  val empty : ('a, 'b) t
  (** get empty environment *)

  val lookup : ('a, 'b) t -> 'a -> 'b
  (** lookup environment : Env.lookup env key => content *)

  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (** id binding : Env.bind env key content => env'*)
end

(** Memory Implementation *)
module Mem : MEM = struct
  exception Not_allocated
  exception Not_initialized

  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list

  let empty = M (Loc.base, [])

  let rec replace_nth l n c =
    match l with
    | h :: t -> if n = 1 then c :: t else h :: replace_nth t (n - 1) c
    | [] -> raise Not_allocated

  let load (M (boundary, storage)) loc =
    match List.nth storage (Loc.diff boundary loc - 1) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary, storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary, storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(** Environment Implementation *)
module Env : ENV = struct
  exception Not_bound

  type ('a, 'b) t = E of ('a -> 'b)

  let empty = E (fun x -> raise Not_bound)
  let lookup (E env) id = env id
  let bind (E env) id loc = E (fun x -> if x = id then loc else env x)
end

(**  K- Interpreter *)
module type KMINUS = sig
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type memory
  type env
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)

  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS = struct
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)

  type memory = value Mem.t

  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | LETF (f, lst, e1, e2) ->
      eval mem (Env.bind env f (Proc (lst, e1, env))) e2
    | CALLV (f, e_lst) ->
    let (x_lst, e, env') = lookup_env_proc env f in
    if List.length x_lst <> List.length e_lst then
      raise (Error "InvalidArg")
    else
    let rec iterate_e mem e_lst v_lst =
      (match e_lst with
      | [] -> (List.rev v_lst, mem)
      | e::e_tail -> 
        let (v1, mem') = eval mem env e in
        iterate_e mem' e_tail (v1::v_lst)
      ) in
    let (v_lst, mem') = iterate_e mem e_lst [] in
    let rec iterate_x mem env x_lst l_lst =
      (match x_lst with
      | [] -> (List.rev l_lst, mem, env)
      | x::x_tail ->
        let (l, mem') = Mem.alloc mem in
        let env' = (Env.bind env x (Addr l)) in
        iterate_x mem' env' x_tail (l::l_lst)
      ) in
    let (l_lst, mem'', env'') = iterate_x mem' env' x_lst [] in 
    let env''' = Env.bind env'' f (Proc (x_lst, e, env'')) in
    let rec iterate_v mem l_lst v_lst =
      (match (l_lst, v_lst) with
      | ([], []) -> mem
      | (l::l_tail, v::v_tail) -> 
        iterate_v (Mem.store mem l v) l_tail v_tail
      | _ -> raise (Error "InvalidArg")
      ) in
    let mem''' = iterate_v mem'' l_lst v_lst in
      eval mem''' env''' e
    | CALLR (f, y_lst) ->
    let (x_lst, e, env') = lookup_env_proc env f in
    if List.length x_lst <> List.length y_lst then
      raise (Error "InvalidArg")
    else
      let rec iterate env' x_lst y_lst =
        (match (x_lst, y_lst) with
        | ([], []) -> env'
        | (x::x_tail, y::y_tail) ->
          iterate (Env.bind env' x (Addr (lookup_env_loc env y))) x_tail y_tail
        | _ -> raise (Error "InvalidArg")
        ) in
      let env'' = iterate env' x_lst y_lst in
      eval mem (Env.bind env'' f (Proc (x_lst, e, env'))) e
    | RECORD xe_lst ->
      if xe_lst = [] then
        (Unit, mem)
      else
      let rec iterate mem xe_lst v_lst l_lst xl_lst =
        (match xe_lst with
        | [] ->
        let r = fun x ->
        try List.assoc x (xl_lst)
          with Not_found -> raise (Error "Unbound") in
        (mem, Record r, List.rev v_lst, List.rev l_lst)
        | (x, e):: tail ->
          let (v, mem') = eval mem env e in
          let (l, mem'') = Mem.alloc mem' in
          iterate mem'' tail (v::v_lst) (l::l_lst) ((x,l)::xl_lst)
        ) in
      let (mem', r, v_lst, l_lst) = iterate mem xe_lst [] [] [] in
      let rec iterate_vl mem v_lst l_lst =
        (match (v_lst, l_lst) with
        | ([], []) -> mem
        | (v::v_tail, l::l_tail) ->
          let mem' = Mem.store mem l v in
          iterate_vl mem' v_tail l_tail
        | _ -> raise (Error "Error")
        ) in
      (r, iterate_vl mem' v_lst l_lst)
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | FIELD (e, x) ->
      let (r, mem') = eval mem env e in
      (Mem.load mem' ((value_record r) x), mem')
    | ASSIGNF (e1, x, e2) ->
      let (r, mem1) = eval mem env e1 in
      let (v, mem2) = eval mem1 env e2 in
      (v, Mem.store mem2 ((value_record r) x) v)
    | NUM n ->
      (Num(n), mem)
    | TRUE ->
      (Bool(true), mem)
    | FALSE ->
      (Bool(false), mem)
    | UNIT ->
      (Unit, mem)
    | VAR x ->
      let l = lookup_env_loc env x in
      (Mem.load mem l, mem)
    | ADD (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (match (v1, v2) with
      | (Num n1, Num n2) -> (Num (n1 + n2), mem2)
      | (_, _) -> raise (Error "TypeError : not int")   
      )
    | SUB (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (match (v1, v2) with
      | (Num n1, Num n2) -> (Num (n1 - n2), mem2)
      | (_, _) -> raise (Error "TypeError : not int")
      )
    | MUL (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (match (v1, v2) with
      | (Num n1, Num n2) -> (Num (n1 * n2), mem2)
      | (_, _) -> raise (Error "TypeError : not int")
      )
    | DIV (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (match (v1, v2) with
      | (Num n1, Num n2) -> (Num (n1 / n2), mem2)
      | (_, _) -> raise (Error "TypeError : not int")
      )
    | EQUAL (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      if v1 = v2 then
        (Bool(true), mem2)
      else 
        (Bool(false), mem2)
    | LESS (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (match (v1, v2) with
      | (Num n1, Num n2) -> (Bool(n1 < n2), mem2)
      | _ -> raise (Error "TypeError: not int")
      )
    | NOT e ->
      let (v1, mem1) = eval mem env e in
      (match v1 with
        | Bool b -> (Bool(not b), mem)
        | _ -> raise (Error "TypeError: not bool")
      )
    | SEQ (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (v2, mem2)
    | IF (e1, e2, e3) ->
      let (v1, mem1) = eval mem env e1 in
      (match v1 with
      | Bool(true) ->
        let (v2, mem2) = eval mem1 env e2 in
        (v2, mem2)
      | Bool(false) ->
        let (v2, mem2) = eval mem1 env e3 in
        (v2, mem2)
      | _ -> raise (Error "TypeError: not bool")
      )
    | WHILE (e1, e2) -> 
      let (v1, mem1) = eval mem env e1 in
      (match v1 with
      | Bool(true) ->
        let (v2, mem2) = eval mem1 env e2 in
        eval mem2 env (WHILE(e1, e2))
      | Bool(false) -> 
        (Unit, mem1)
      | _ -> raise (Error "TypeError: not bool")
      )
  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
