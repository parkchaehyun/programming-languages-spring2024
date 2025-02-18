type crazy3 = NIL | ZERO of crazy3
| ONE of crazy3 | MONE of crazy3
| TWO of crazy3 | MTWO of crazy3

let rec crazy3add (c1, c2) =
  match (c1, c2) with
  | (NIL, c2) -> c2
  | (c1, NIL) -> c1
  | (TWO(c3), TWO(c4)) -> ONE(crazy3add(ONE(NIL), crazy3add(c3, c4)))
  | (MTWO(c3), MTWO(c4)) -> MONE(crazy3add(MONE(NIL), crazy3add(c3, c4)))
  | (ONE(c3), TWO(c4)) -> ZERO(crazy3add(ONE(NIL), crazy3add(c3, c4)))
  | (TWO(c3), ONE(c4)) -> ZERO(crazy3add(ONE(NIL), crazy3add(c3, c4)))
  | (MONE(c3), MTWO(c4)) -> ZERO(crazy3add(MONE(NIL), crazy3add(c3, c4)))
  | (MTWO(c3), MONE(c4)) -> ZERO(crazy3add(MONE(NIL), crazy3add(c3, c4)))
  | (TWO(c3), ZERO(c4)) -> TWO(crazy3add(c3, c4))
  | (ONE(c3), ONE(c4)) -> TWO(crazy3add(c3, c4))
  | (ZERO(c3), TWO(c4)) -> TWO(crazy3add(c3, c4))
  | (MTWO(c3), ZERO(c4)) -> MTWO(crazy3add(c3, c4))
  | (MONE(c3), MONE(c4)) -> MTWO(crazy3add(c3, c4))
  | (ZERO(c3), MTWO(c4)) -> MTWO(crazy3add(c3, c4))
  | (MONE(c3), TWO(c4)) -> ONE(crazy3add(c3, c4))
  | (ZERO(c3), ONE(c4)) -> ONE(crazy3add(c3, c4))
  | (ONE(c3), ZERO(c4)) -> ONE(crazy3add(c3, c4))
  | (TWO(c3), MONE(c4)) -> ONE(crazy3add(c3, c4))
  | (MTWO(c3), ONE(c4)) -> MONE(crazy3add(c3, c4))
  | (MONE(c3), ZERO(c4)) -> MONE(crazy3add(c3, c4))
  | (ZERO(c3), MONE(c4)) -> MONE(crazy3add(c3, c4))
  | (ONE(c3), MTWO(c4)) -> MONE(crazy3add(c3, c4))
  | (ZERO(c3), ZERO(c4)) -> ZERO(crazy3add(c3, c4))
  | (ONE(c3), MONE(c4)) -> ZERO(crazy3add(c3, c4))
  | (MONE(c3), ONE(c4)) -> ZERO(crazy3add(c3, c4))
  | (TWO(c3), MTWO(c4)) -> ZERO(crazy3add(c3, c4))
  | (MTWO(c3), TWO(c4)) -> ZERO(crazy3add(c3, c4))
