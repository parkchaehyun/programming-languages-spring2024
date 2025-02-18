type crazy3 = NIL | ZERO of crazy3
| ONE of crazy3 | MONE of crazy3
| TWO of crazy3 | MTWO of crazy3

let rec crazy3val crazy3 =
  match crazy3 with
  | NIL -> 0
  | ZERO(c) ->
    3 * crazy3val(c)
  | ONE(c) ->
    1 + 3 * crazy3val(c)
  | MONE(c) ->
    -1 + 3 * crazy3val(c)
  | TWO(c) ->
    2 + 3 * crazy3val(c)
  | MTWO(c) ->
    -2 + 3 * crazy3val(c)

