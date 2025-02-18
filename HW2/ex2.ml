type metro = STATION of name
          | AREA of name * metro
          | CONNECT of metro * metro
          and name = string

let rec checkArea areas metro = match metro with
  | STATION s -> 
    if List.mem s areas then true else false
  | AREA (id, m) ->
    checkArea (id::areas) m
  | CONNECT (m1, m2) ->
    checkArea areas m1 && checkArea areas m2

let checkMetro metro = match metro with
  | STATION _ -> false 
  | AREA (id, m) ->
    checkArea [id] m
  | CONNECT (m1, m2) ->
    checkArea [] m1 && checkArea [] m2