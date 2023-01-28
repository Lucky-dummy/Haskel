euler 1 = 1
euler n = euler' n 2 where
  euler' n k = if k < n
    then if gcd n k == 1
      then 1 + euler' n (k+1)
      else euler' n (k+1)
    else 1