
(* The type of tokens. *)

type token = 
  | WRITE
  | VAL
  | TRUE
  | THEN
  | STRING of (string)
  | SEMICOLON
  | RP
  | REC
  | READ
  | RARROW
  | PLUS
  | OR
  | NUM of (int)
  | MINUS
  | MALLOC
  | LP
  | LET
  | IN
  | IF
  | ID of (string)
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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Nutritious__M.M.exp)
