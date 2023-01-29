{-
Выходные и праздничные дни заданы списком пар (день, месяц). Определить, в
каком месяце больше всего нерабочих дней.
-}

foo [] = error "Empty list"
foo lst = snd (maxP (goo lst 1)) where
  goo lst n
    | n == 13 = []
    | otherwise =
      foldl
      (\ acc x
        -> if snd acc == snd x then (fst acc + 1, snd acc) else acc)
      (0, n) lst
      : goo lst (n + 1)
  maxP lst = maxP' lst (head lst)
  maxP' [] mx = mx
  maxP' (x : xs) mx = if fst x > fst mx
      then maxP' xs x
      else maxP' xs mx

{-
[(1, 1), (23, 2), (8, 3), (1, 5), (9, 5), (12, 6), (4, 11)]
-}