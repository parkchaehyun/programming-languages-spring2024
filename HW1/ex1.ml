let rec sigma (a, b, n) = 
  if a > b then 0
  else if a = b then n a
  else n a + sigma (a+1, b, n)