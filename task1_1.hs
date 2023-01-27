foo n m = div (fact(m+n-1)) (fact(n-1) * fact(m)) where
  fact k = product[1..k]