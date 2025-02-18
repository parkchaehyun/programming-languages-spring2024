
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WRITE
    | WHILE
    | UNIT
    | TRUE
    | TO
    | THEN
    | STAR
    | SLASH
    | SEMICOLON
    | RP
    | READ
    | RBLOCK
    | RB
    | PROC
    | PLUS
    | NUM of (
# 19 "lib/parser.mly"
       (int)
# 29 "lib/parser.ml"
  )
    | NOT
    | MINUS
    | LP
    | LET
    | LBLOCK
    | LB
    | IN
    | IF
    | ID of (
# 21 "lib/parser.mly"
       (string)
# 42 "lib/parser.ml"
  )
    | FOR
    | FALSE
    | EQUAL
    | EOF
    | END
    | ELSE
    | DO
    | COLONEQ
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState67
  | MenhirState61
  | MenhirState58
  | MenhirState54
  | MenhirState52
  | MenhirState49
  | MenhirState45
  | MenhirState43
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState25
  | MenhirState22
  | MenhirState20
  | MenhirState19
  | MenhirState11
  | MenhirState8
  | MenhirState2
  | MenhirState1
  | MenhirState0

# 6 "lib/parser.mly"
  
type declLet =
  | Val of string * K.exp
  | Fun of string * string * K.exp

let desugarLet: declLet * K.exp -> K.exp  =
 fun (l, e) ->
 	match l with
  | Val(x, e') -> K.LETV(x,e',e)
 	| Fun(f,x,e') -> K.LETF(f,x,e',e)

# 102 "lib/parser.ml"

let rec _menhir_goto_decl : _menhir_env -> 'ttv_tail -> (declLet) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ID _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LP ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NOT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NUM _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | READ ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | UNIT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | WHILE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Sm5__K.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Sm5__K.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Sm5__K.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Sm5__K.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Sm5__K.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Sm5__K.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Sm5__K.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Sm5__K.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | FOR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | ID _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | LET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | LP ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | MINUS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | NOT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | READ ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | UNIT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | WHILE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | FOR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | ID _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LP ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | MINUS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | NOT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | READ ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | UNIT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | WHILE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Sm5__K.program))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
        let _v : (Sm5__K.program) = 
# 59 "lib/parser.mly"
                   ( K.MUL (_1,_3) )
# 556 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Sm5__K.program))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
        let _v : (Sm5__K.program) = 
# 60 "lib/parser.mly"
                    ( K.DIV (_1,_3) )
# 566 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | IN | RB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Sm5__K.program))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 66 "lib/parser.mly"
                        ( K.SEQ (_1,_3) )
# 592 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | EQUAL | IN | LB | MINUS | PLUS | RB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Sm5__K.program))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 57 "lib/parser.mly"
                   ( K.ADD (_1, _3) )
# 616 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | EQUAL | IN | LB | MINUS | PLUS | RB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Sm5__K.program))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 58 "lib/parser.mly"
                     (K.SUB (_1,_3) )
# 640 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Sm5__K.program))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 63 "lib/parser.mly"
                    ( match (_1,_3) with (K.VAR f, K.VAR x) -> K.CALLR (f, x) | _ -> raise Parsing.Parse_error)
# 666 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | EQUAL | IN | LB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Sm5__K.program))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 62 "lib/parser.mly"
                 ( K.LESS (_1,_3) )
# 679 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | EQUAL | IN | LB | RB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Sm5__K.program))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 61 "lib/parser.mly"
                    ( K.EQUAL (_1,_3) )
# 707 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | IN | RB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (_2 : (
# 21 "lib/parser.mly"
       (string)
# 738 "lib/parser.ml"
            ))), _, (_4 : (Sm5__K.program))), _, (_6 : (Sm5__K.program))), _, (_8 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 69 "lib/parser.mly"
                                        ( K.FOR (_2, _4, _6, _8) )
# 743 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (
# 21 "lib/parser.mly"
       (string)
# 772 "lib/parser.ml"
            ))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 56 "lib/parser.mly"
                  ( K.CALLV (_1, _3) )
# 777 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | IN | RB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (
# 21 "lib/parser.mly"
       (string)
# 814 "lib/parser.ml"
            ))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 65 "lib/parser.mly"
                    ( K.ASSIGN (_1,_3) )
# 819 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | FOR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | ID _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LP ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | MINUS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | NOT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | READ ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | UNIT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | WHILE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | FOR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | ID _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LP ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | MINUS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | NOT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | READ ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | UNIT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | WHILE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | IN | RB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Sm5__K.program))), _, (_4 : (Sm5__K.program))), _, (_6 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 67 "lib/parser.mly"
                                ( K.IF (_2, _4, _6) )
# 975 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, (_2 : (
# 21 "lib/parser.mly"
       (string)
# 1008 "lib/parser.ml"
            ))), (_4 : (
# 21 "lib/parser.mly"
       (string)
# 1012 "lib/parser.ml"
            ))), _, (_7 : (Sm5__K.program))) = _menhir_stack in
            let _v : (declLet) = 
# 76 "lib/parser.mly"
                                (Fun (_2, _4, _7))
# 1017 "lib/parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (_1 : (
# 21 "lib/parser.mly"
       (string)
# 1050 "lib/parser.ml"
            ))), _, (_3 : (Sm5__K.program))) = _menhir_stack in
            let _v : (declLet) = 
# 75 "lib/parser.mly"
                    ( Val (_1, _3) )
# 1055 "lib/parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | IN | RB | RP | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (_2 : (declLet))), _, (_4 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 70 "lib/parser.mly"
                     ( desugarLet(_2, _4) )
# 1089 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 48 "lib/parser.mly"
               ( _2 )
# 1119 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Sm5__K.program))) = _menhir_stack in
        let _v : (Sm5__K.program) = 
# 64 "lib/parser.mly"
             ( K.NOT (_2) )
# 1141 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | FOR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | ID _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LP ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | MINUS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NOT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | READ ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | UNIT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | WHILE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | IN | RB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Sm5__K.program))), _, (_4 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 68 "lib/parser.mly"
                       ( K.WHILE (_2, _4) )
# 1229 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | EOF | IN | RB | RP | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 72 "lib/parser.mly"
               ( K.WRITE (_2) )
# 1261 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Sm5__K.program))) = _menhir_stack in
            let _v : (Sm5__K.program) = 
# 45 "lib/parser.mly"
             ( _1 )
# 1282 "lib/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Sm5__K.program)) = _v in
            Obj.magic _1
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Sm5__K.program) = 
# 49 "lib/parser.mly"
         (K.UNIT)
# 1491 "lib/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Sm5__K.program) = 
# 52 "lib/parser.mly"
         ( K.TRUE )
# 1502 "lib/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (
# 21 "lib/parser.mly"
       (string)
# 1519 "lib/parser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Sm5__K.program) = 
# 71 "lib/parser.mly"
            ( K.READ (_2) )
# 1525 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "lib/parser.mly"
       (int)
# 1538 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 19 "lib/parser.mly"
       (int)
# 1546 "lib/parser.ml"
    )) = _v in
    let _v : (Sm5__K.program) = 
# 51 "lib/parser.mly"
        ( K.NUM (_1) )
# 1551 "lib/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NUM _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (
# 19 "lib/parser.mly"
       (int)
# 1607 "lib/parser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Sm5__K.program) = 
# 50 "lib/parser.mly"
              ( K.NUM (-_2) )
# 1613 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | RP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState11 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Sm5__K.program) = 
# 54 "lib/parser.mly"
          ( K.UNIT )
# 1658 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLONEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | FOR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | ID _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | LET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | LP ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | MINUS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NOT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | READ ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | UNIT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | WHILE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | PROC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LP ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | RP ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | EQUAL ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | FALSE ->
                                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | FOR ->
                                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | ID _v ->
                                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
                            | IF ->
                                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | LET ->
                                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | LP ->
                                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | MINUS ->
                                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | NOT ->
                                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | NUM _v ->
                                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
                            | READ ->
                                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | TRUE ->
                                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | UNIT ->
                                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | WHILE ->
                                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | WRITE ->
                                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "lib/parser.mly"
       (string)
# 1866 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLONEQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | ID _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LP ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MINUS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NOT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NUM _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | READ ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | UNIT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | WHILE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | LP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | FOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | ID _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | LET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | LP ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | MINUS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | NOT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | NUM _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | READ ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | UNIT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | WHILE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | DO | ELSE | EOF | EQUAL | IN | LB | MINUS | PLUS | RB | RP | SEMICOLON | SLASH | STAR | THEN | TO ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (
# 21 "lib/parser.mly"
       (string)
# 1952 "lib/parser.ml"
        ))) = _menhir_stack in
        let _v : (Sm5__K.program) = 
# 55 "lib/parser.mly"
       ( K.VAR (_1) )
# 1957 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLONEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | FOR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | ID _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LP ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | MINUS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NOT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | READ ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | UNIT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | WHILE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Sm5__K.program) = 
# 53 "lib/parser.mly"
          ( K.FALSE )
# 2036 "lib/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Sm5__K.program) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FOR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LP ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MINUS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | READ ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNIT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WHILE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 78 "lib/parser.mly"
  

# 2100 "lib/parser.ml"

# 269 "<standard.mly>"
  

# 2105 "lib/parser.ml"
