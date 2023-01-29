{-
Дан список участников круговой считалки, в которой на каждом шаге выбывает
каждый пятый. Определить, кто останется в списке последним.
-}

countingRhyme l = counting l [] 1 where
  counting [] [a] _ = a
  counting [] reslist num = counting reslist [] num
  counting (h:list) reslist num
    | num == 5 = counting list reslist 1
    | otherwise = counting list (reslist ++ [h]) (num+1)

{-
  [1, 2] -> 2
  [1, 2, 3] -> 1
  [1, 2, 3, 4] -> 2
  [1, 2, 3, 4, 5] -> 2
-}