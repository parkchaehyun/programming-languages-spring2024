
(* The type of tokens. *)

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
  | NUM of (int)
  | NOT
  | MINUS
  | LP
  | LET
  | LBLOCK
  | LB
  | IN
  | IF
  | ID of (string)
  | FOR
  | FALSE
  | EQUAL
  | EOF
  | END
  | ELSE
  | DO
  | COLONEQ

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Sm5__K.program)
