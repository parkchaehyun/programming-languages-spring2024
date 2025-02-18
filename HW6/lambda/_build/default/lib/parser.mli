
(* The type of tokens. *)

type token = 
  | RP
  | LP
  | LET
  | LAMBDA
  | IN
  | ID of (string)
  | EQUAL
  | EOF
  | DOT
  | APP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lambda__Lexp.t_let)
