{-
Родственные отношения заданы списком пар (отец, сын). Определить всех
потомков (сыновей, внуков, правнуков и т.д.) указанного индивида.
-}

descendants l p
  | null l = []
  | fst (head l) == p
    = snd (head l) : descendants (tail l) p ++ descendants l (snd (head l))
  | otherwise = descendants (tail l) p

{-
  [("Grandpa", "Father"), ("Father", "Son"), ("Son", "Grandson")]
-}