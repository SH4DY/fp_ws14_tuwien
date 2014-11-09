Literate Haskell - Ass 3 - Ramon Lopez

Es wird stets der Listenkopf mit dem ersten Element verglichen (sind beides Nullen?)
Falls eine Mehrfachnull entdeckt wird - sofort mit False abbrechen
Falls keine Mehrfachnullfolge entdeckt wird, wird die Liste bis zum Ende durchlaufen
> zeroTest :: [Integer] -> Bool
> zeroTest (x:xs)
>    | x == 0 && (head xs) == 0 = False
>    | xs == [] = True
>    | otherwise = zeroTest xs

numberOf :: Integer -> Integer
