module type Queue =
sig
type element
type queue
exception EMPTY_Q
val emptyQ: queue
val enQ: queue * element -> queue
val deQ: queue -> element * queue
end

module IntListQ =
struct
type element = int list
type queue = int list list * int list list
exception EMPTY_Q
let emptyQ = ([], [])
let enQ ((l, r), e) = ((e :: l, r))
let deQ (l, r) = 
  if r = [] then
    if l = [] then raise EMPTY_Q 
    else
      let rev = List.rev l in
      (List.hd rev, ([], List.tl rev)) 
  else
    (List.hd r, (l, (List.tl r)))
end