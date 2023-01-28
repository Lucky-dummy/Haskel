-- least common multiple
foo a b = if a < b
  then div b (gcd b a) * a
  else div a (gcd a b) * b where
    gcd x y = if y > 0
      then gcd y (rem x y)
      else x