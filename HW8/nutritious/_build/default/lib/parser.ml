
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WRITE
    | VAL
    | TRUE
    | THEN
    | STRING of (
# 23 "lib/parser.mly"
       (string)
# 18 "lib/parser.ml"
  )
    | SEMICOLON
    | RP
    | REC
    | READ
    | RARROW
    | PLUS
    | OR
    | NUM of (
# 21 "lib/parser.mly"
       (int)
# 30 "lib/parser.ml"
  )
    | MINUS
    | MALLOC
    | LP
    | LET
    | IN
    | IF
    | ID of (
# 22 "lib/parser.mly"
       (string)
# 41 "lib/parser.ml"
  )
    | FN
    | FALSE
    | EQUAL
    | EOF
    | END
    | ELSE
    | DOT
    | COMMA
    | COLONEQ
    | BANG
    | AND
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState66
  | MenhirState64
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState58
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState25
  | MenhirState24
  | MenhirState19
  | MenhirState18
  | MenhirState16
  | MenhirState12
  | MenhirState11
  | MenhirState8
  | MenhirState7
  | MenhirState6
  | MenhirState1
  | MenhirState0

# 7 "lib/parser.mly"
  
exception EmptyBinding
let rec desugarLet =
  function ([], e) -> raise EmptyBinding
   | (a::[], e) -> M.M.LET(a,e)
   | (a::r, e) -> M.M.LET(a, desugarLet(r,e))

exception IncorrectSelection
let whichSel = function (e, 1) -> M.M.FST e
       | (e, 2) -> M.M.SND e
       | _ -> raise IncorrectSelection

# 123 "lib/parser.ml"

let rec _menhir_goto_decls : _menhir_env -> 'ttv_tail -> _menhir_state -> (Nutritious__M.M.decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState52 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | REC ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | VAL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Nutritious__M.M.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Nutritious__M.M.decl)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.decl list))) = _menhir_stack in
        let _v : (Nutritious__M.M.decl list) = 
# 72 "lib/parser.mly"
                 (_1 @ [_2])
# 189 "lib/parser.ml"
         in
        _menhir_goto_decls _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Nutritious__M.M.decl)) = _v in
        let _v : (Nutritious__M.M.decl list) = 
# 71 "lib/parser.mly"
            ([_1])
# 199 "lib/parser.ml"
         in
        _menhir_goto_decls _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Nutritious__M.M.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Nutritious__M.M.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Nutritious__M.M.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Nutritious__M.M.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Nutritious__M.M.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Nutritious__M.M.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Nutritious__M.M.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Nutritious__M.M.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NUM _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (
# 21 "lib/parser.mly"
       (int)
# 477 "lib/parser.ml"
        )) = _v in
        let ((_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))), _) = _menhir_stack in
        let _v : (Nutritious__M.M.exp) = 
# 54 "lib/parser.mly"
                   (whichSel (_1,_3))
# 483 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Nutritious__M.M.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | AND | COLONEQ | COMMA | ELSE | END | EOF | EQUAL | FN | IF | IN | LET | MINUS | OR | PLUS | REC | RP | SEMICOLON | THEN | VAL | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 68 "lib/parser.mly"
                (M.M.BANG (_2))
# 533 "lib/parser.ml"
             in
            _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | COMMA | ELSE | END | EOF | IN | REC | RP | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (_2 : (
# 22 "lib/parser.mly"
       (string)
# 590 "lib/parser.ml"
            ))), _, (_4 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 63 "lib/parser.mly"
                        (M.M.FN(_2,_4))
# 595 "lib/parser.ml"
             in
            _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | COLONEQ | COMMA | ELSE | END | EOF | EQUAL | FN | IF | IN | LET | MINUS | OR | PLUS | REC | RP | SEMICOLON | THEN | VAL | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))), _), _, (_3 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 47 "lib/parser.mly"
                     (M.M.BOP(M.M.ADD,_1,_3))
# 635 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | AND | COLONEQ | COMMA | ELSE | END | EOF | EQUAL | FN | IF | IN | LET | MINUS | OR | PLUS | REC | RP | SEMICOLON | THEN | VAL | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))), _), _, (_3 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 50 "lib/parser.mly"
                    (M.M.BOP(M.M.AND,_1,_3))
# 673 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | COLONEQ | COMMA | ELSE | END | EOF | EQUAL | FN | IF | IN | LET | MINUS | OR | PLUS | REC | RP | SEMICOLON | THEN | VAL | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))), _), _, (_3 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 51 "lib/parser.mly"
                   (M.M.BOP(M.M.OR,_1,_3))
# 713 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | COLONEQ | COMMA | ELSE | END | EOF | EQUAL | FN | IF | IN | LET | MINUS | OR | PLUS | REC | RP | SEMICOLON | THEN | VAL | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))), _), _, (_3 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 48 "lib/parser.mly"
                      (M.M.BOP(M.M.SUB,_1,_3))
# 753 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | COLONEQ | COMMA | ELSE | END | EOF | EQUAL | FN | IF | IN | LET | REC | RP | SEMICOLON | THEN | VAL | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))), _), _, (_3 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 49 "lib/parser.mly"
                      (M.M.BOP(M.M.EQ,_1,_3))
# 799 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | COMMA | ELSE | END | EOF | FN | IN | LET | REC | RP | SEMICOLON | THEN | VAL | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))), _), _, (_3 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 53 "lib/parser.mly"
                        (M.M.ASSIGN(_1,_3))
# 851 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | SEMICOLON ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState37 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FALSE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | ID _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | IF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | MALLOC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NUM _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | READ ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState39 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | FALSE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | FN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | ID _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | IF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | LET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | LP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | MALLOC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | NUM _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | READ ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | SEMICOLON ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | COMMA | ELSE | END | EOF | IN | REC | RP | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))), _), _, (_3 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 52 "lib/parser.mly"
                          (M.M.SEQ (_1,_3))
# 1085 "lib/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | COLONEQ | COMMA | ELSE | END | EOF | FN | IN | LET | REC | RP | SEMICOLON | THEN | VAL | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (_2 : (Nutritious__M.M.exp))), _), _, (_4 : (Nutritious__M.M.exp))), _), _, (_6 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 65 "lib/parser.mly"
                                  (M.M.IF(_2,_4,_6))
# 1133 "lib/parser.ml"
             in
            _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | SEMICOLON ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | IN | REC | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (_2 : (
# 22 "lib/parser.mly"
       (string)
# 1192 "lib/parser.ml"
            ))), _, (_4 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.decl) = 
# 74 "lib/parser.mly"
                        (M.M.VAL(_2, _4))
# 1197 "lib/parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SEMICOLON ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | IN | REC | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (_2 : (
# 22 "lib/parser.mly"
       (string)
# 1256 "lib/parser.ml"
            ))), (_5 : (
# 22 "lib/parser.mly"
       (string)
# 1260 "lib/parser.ml"
            ))), _, (_7 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.decl) = 
# 75 "lib/parser.mly"
                                     (M.M.REC(_2, _5, _7))
# 1265 "lib/parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Nutritious__M.M.decl list))), _), _, (_4 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 64 "lib/parser.mly"
                            (desugarLet(_2,_4))
# 1294 "lib/parser.ml"
             in
            _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SEMICOLON ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState58 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | FALSE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | FN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | ID _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | IF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | LET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | LP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | MALLOC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | NUM _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | READ ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState58 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 56 "lib/parser.mly"
                  (_2)
# 1420 "lib/parser.ml"
             in
            _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState61 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Nutritious__M.M.exp))), _), _, (_4 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 69 "lib/parser.mly"
                            (M.M.PAIR (_2,_4))
# 1483 "lib/parser.ml"
             in
            _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | AND | COLONEQ | COMMA | ELSE | END | EOF | EQUAL | FN | IF | IN | LET | MINUS | OR | PLUS | REC | RP | SEMICOLON | THEN | VAL | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 67 "lib/parser.mly"
                  (M.M.MALLOC (_2))
# 1529 "lib/parser.ml"
             in
            _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | COMMA | ELSE | END | EOF | FN | IN | LET | REC | RP | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 66 "lib/parser.mly"
                 (M.M.WRITE (_2))
# 1583 "lib/parser.ml"
             in
            _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | BANG ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | COLONEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | DOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState66 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))) = _menhir_stack in
            let _v : (Nutritious__M.M.exp) = 
# 43 "lib/parser.mly"
                    (_1)
# 1611 "lib/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Nutritious__M.M.exp)) = _v in
            Obj.magic _1
        | EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | FALSE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | FN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MALLOC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | NUM _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | READ ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SEMICOLON ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | WRITE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | _ ->
        _menhir_fail ()

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | FALSE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | FN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | ID _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | IF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | LET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | LP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | MALLOC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | NUM _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | READ ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
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

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FN ->
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
                    | RARROW ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BANG ->
                            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | FALSE ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | FN ->
                            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | ID _v ->
                            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
                        | IF ->
                            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | LET ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | LP ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | MALLOC ->
                            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | NUM _v ->
                            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
                        | READ ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | STRING _v ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
                        | TRUE ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | WRITE ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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

and _menhir_goto_aexpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Nutritious__M.M.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState66 | MenhirState64 | MenhirState63 | MenhirState58 | MenhirState61 | MenhirState54 | MenhirState51 | MenhirState44 | MenhirState37 | MenhirState39 | MenhirState43 | MenhirState41 | MenhirState24 | MenhirState36 | MenhirState34 | MenhirState32 | MenhirState30 | MenhirState26 | MenhirState28 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Nutritious__M.M.exp)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Nutritious__M.M.exp))) = _menhir_stack in
        let _v : (Nutritious__M.M.exp) = 
# 46 "lib/parser.mly"
                 (M.M.APP(_1,_2))
# 1823 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState1 | MenhirState6 | MenhirState7 | MenhirState60 | MenhirState53 | MenhirState50 | MenhirState11 | MenhirState12 | MenhirState38 | MenhirState42 | MenhirState40 | MenhirState16 | MenhirState35 | MenhirState33 | MenhirState31 | MenhirState29 | MenhirState25 | MenhirState27 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Nutritious__M.M.exp)) = _v in
        let _v : (Nutritious__M.M.exp) = 
# 45 "lib/parser.mly"
            (_1)
# 1833 "lib/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
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
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Nutritious__M.M.exp) = 
# 59 "lib/parser.mly"
           (M.M.CONST(M.M.B true))
# 2054 "lib/parser.ml"
     in
    _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "lib/parser.mly"
       (string)
# 2061 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 23 "lib/parser.mly"
       (string)
# 2069 "lib/parser.ml"
    )) = _v in
    let _v : (Nutritious__M.M.exp) = 
# 58 "lib/parser.mly"
             (M.M.CONST(M.M.S _1))
# 2074 "lib/parser.ml"
     in
    _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Nutritious__M.M.exp) = 
# 62 "lib/parser.mly"
           (M.M.READ)
# 2085 "lib/parser.ml"
     in
    _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "lib/parser.mly"
       (int)
# 2092 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 21 "lib/parser.mly"
       (int)
# 2100 "lib/parser.ml"
    )) = _v in
    let _v : (Nutritious__M.M.exp) = 
# 57 "lib/parser.mly"
          (M.M.CONST(M.M.N _1))
# 2105 "lib/parser.ml"
     in
    _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | VAL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "lib/parser.mly"
       (string)
# 2238 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 22 "lib/parser.mly"
       (string)
# 2246 "lib/parser.ml"
    )) = _v in
    let _v : (Nutritious__M.M.exp) = 
# 61 "lib/parser.mly"
         (M.M.VAR(_1))
# 2251 "lib/parser.ml"
     in
    _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | FALSE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | FN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | ID _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | IF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | MALLOC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | NUM _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | READ ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | WRITE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
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

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Nutritious__M.M.exp) = 
# 60 "lib/parser.mly"
            (M.M.CONST(M.M.B false))
# 2322 "lib/parser.ml"
     in
    _menhir_goto_aexpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

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

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Nutritious__M.M.exp) =
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
    | BANG ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MALLOC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NUM _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | READ ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WRITE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 77 "lib/parser.mly"
  

# 2421 "lib/parser.ml"

# 269 "<standard.mly>"
  

# 2426 "lib/parser.ml"
