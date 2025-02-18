type formula = TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr

and  expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec return_int expr =
  match expr with
  | NUM (i) -> i
  | PLUS (j, k) -> return_int(j) + return_int(k)
  | MINUS (j, k) -> return_int(j) - return_int(k)  

let rec eval formula =
  match formula with
  | FALSE -> false
  | TRUE -> true
  | NOT (formula) ->
    if (eval formula = true) then false
    else true
  | ANDALSO (a, b)  ->
    if (eval a = false) then false
    else if (eval b = false) then false
    else true
  | ORELSE (a, b) ->
    if (eval a = true) then true
    else if (eval b = true) then true
    else false
  | IMPLY (a, b) ->
    if (eval a = true) then
      if (eval b = false) then false
      else true
    else true
  | LESS (a, b) ->
    if (return_int(a) < return_int(b)) then true
    else false