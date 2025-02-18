(*
 * SNU 4190.310 Programming Languages 2024 Spring
 * Homework "SM5"
 *)

(* TODO : complete this function *)
let rec trans : K.program -> Machine.command = function
  | K.NUM i -> [ Machine.PUSH (Machine.Val (Machine.Z i)) ]
  | K.TRUE ->
  [
    Machine.PUSH (Machine.Val (Machine.B true))
  ]
  | K.FALSE ->
  [
    Machine.PUSH (Machine.Val (Machine.B false))
  ]
  | K.UNIT ->
  [
    Machine.PUSH (Machine.Val Unit)
  ]
  | K.VAR id ->
    [
      Machine.PUSH (Machine.Id id);
      Machine.LOAD;
    ]

  | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [ Machine.ADD; ]
  | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [ Machine.SUB; ]
  | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [ Machine.MUL; ]
  | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [ Machine.DIV; ]
  | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [ Machine.EQ; ]
  | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [ Machine.LESS; ]
  | K.NOT (e1) -> trans e1 @ [ Machine.NOT; ]
  | K.READ x ->
  [
    Machine.GET; (* read and push v *)
    Machine.PUSH (Machine.Id x);  (* push l *)
    Machine.STORE; (* map v to l *)
    Machine.PUSH (Machine.Id x); (* push l *)
    Machine.LOAD; (* read from l; get read value *)
  ]
  | K.WRITE x ->
    [
      Machine.MALLOC;
      Machine.BIND "@";
    ] @ trans x
    @ [
      Machine.PUSH (Machine.Id "@");
      Machine.STORE;
      Machine.PUSH (Machine.Id "@");
      Machine.LOAD;
      Machine.PUT;
      Machine.PUSH (Machine.Id "@");
      Machine.LOAD;
      Machine.UNBIND;
      Machine.POP;
    ]
    

  | K.LETV (x, e1, e2) ->
      trans e1 (* push v *)
      @ [
          Machine.MALLOC; (* push loc *)
          Machine.BIND x; (* (x, loc) bound in env *)
          Machine.PUSH (Machine.Id x); (* push x *)
          Machine.STORE; (* map x -> v *)
        ]
      @ trans e2
      @ [ Machine.UNBIND; Machine.POP ] (*unmap x-> v, push x->v, pop x->v*)
  | K.LETF (f, x, e1, e2) ->
    [
        Machine.PUSH (Machine.Fn (x, [Machine.BIND f] @ trans e1));
        Machine.BIND f;
    ]
    @ trans e2
    @ [Machine.UNBIND; Machine.POP]
  | K.ASSIGN (x, e) ->
    trans e (* push v *)
    @ [
      Machine.PUSH (Machine.Id x);
      Machine.STORE;
      Machine.PUSH (Machine.Id x);
      Machine.LOAD;
    ]
  | K.IF (e_cond, e_true, e_false) ->
    trans e_cond @
    [ Machine.JTR (trans e_true, trans e_false);]
  | K.WHILE (e_cond, e_body) ->
  trans(
    K.LETF("@while", "@x",
      K.IF(K.VAR ("@x"), K.SEQ(e_body, K.CALLV("@while", e_cond)), K.UNIT),
      K.CALLV("@while", e_cond))
  )
  | K.FOR (id, e1, e2, e_body) ->
  trans(
    K.LETV("@n1", e1,
    K.LETV("@n2", K.ADD(e2, K.NUM(1)),
    K.LETF("@for", "@n1",
    K.IF(K.LESS(K.VAR("@n1"), K.VAR("@n2")),
    K.SEQ(K.ASSIGN(id, K.VAR("@n1")),
    K.SEQ(e_body, K.CALLV("@for", K.ADD(K.VAR("@n1"), K.NUM (1))))), K.UNIT),
    K.CALLV("@for", K.VAR("@n1")))))
    )


    (* trans (K.LESS (e1, e2)) @ [ Machine.JTR ((trans e_body @ ), [Machine.PUSH (Machine.Val Unit)]))] *)
    (* trans e1 (* push n1 *)
    @ [ 
      Machine.PUSH (Machine.Val (Machine.Z 1));
      Machine.SUB; (* n1-1 *)
      Machine.MALLOC; (* push loc *)
      Machine.BIND id; (* (id, loc) bound in env *)
      Machine.PUSH (Machine.Id id); (* push loc *)
      Machine.STORE; (* map id -> v *)
      Machine.PUSH (Machine.Id id); (* push loc *)
      Machine.LOAD;
    ]
    @ trans e2
    @ [
      Machine.MALLOC; (* push loc *)
      Machine.BIND (id ^ "@"); (* (id, loc) bound in env *)
      Machine.PUSH (Machine.Id id); (* push loc *)
      Machine.STORE; (* map loc -> v *)
      Machine.PUSH (Machine.Id id); (* push loc *)
      Machine.LOAD;
    ]
    @ [ 
      Machine.LESS;
      Machine.JTR (forhelp (id), [Machine.PUSH (Machine.Val Unit)]);
    ] *)
  | K.SEQ (e1, e2) ->
    trans e1 @ trans e2
  | K.CALLV(f, arg_exp) ->
    [
        Machine.PUSH (Machine.Id f);
        Machine.PUSH (Machine.Id f);
    ]
    @ trans arg_exp
    @ [
      Machine.MALLOC;
      Machine.CALL;
    ]
  | K.CALLR(f, arg_var) ->
  [
    Machine.PUSH (Machine.Id f);
    Machine.PUSH (Machine.Id f); (* proc *)
    Machine.PUSH (Machine.Id arg_var); (* arg_var's address l*)
    Machine.LOAD; (* arg_var's value v *)
    Machine.PUSH (Machine.Id arg_var); (* arg_var's address l*)
    Machine.CALL;
  ]
  
  and forhelp id = 
    [ 
      Machine.PUSH (Machine.Id id);
      Machine.LOAD;
      Machine.PUSH (Machine.Val (Machine.Z 1));
      Machine.ADD;
      Machine.PUSH (Machine.Id id); (* push id *)
      Machine.STORE; (* map id -> v *)
      Machine.PUSH (Machine.Id id); (* push id *)
      Machine.LOAD;
      Machine.PUSH (Machine.Id (id ^ "@")); (* push id *)
      Machine.LOAD;
      Machine.LESS;
      Machine.JTR (forhelp (id), [Machine.PUSH (Machine.Val Unit)]);
    ]