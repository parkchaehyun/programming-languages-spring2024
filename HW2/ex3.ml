type heap = EMPTY 
          | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with | EMPTY -> -1
| NODE(r,_,_,_) -> r

let rec merge (a, b) = match (a, b) with
  | (EMPTY, _) -> b
  | (_, EMPTY) -> a
  | (NODE(x, vx, lhx, rhx), NODE(y, vy, lhy, rhy)) ->
    let shake (x,lh,rh) = if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)
    in if vx < vy then shake (vx, lhx, merge(rhx, b))
    else shake (vy, lhy, merge (a, rhy))

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x

let deleteMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,lh,rh) -> merge(lh,rh)

