-- quadratic equation roots
root a b c
    | d < 0 = error "discriminant less than zero" -- or d < 0 = []
    | a == 0 = [- c / b]
    | d == 0 = [- b / (2 * a)]
    | otherwise = [x , y] where
        d = b * b - 4 * a * c
        x = (-b + sqrt d) / (2 * a)
        y = (-b - sqrt d) / (2 * a)