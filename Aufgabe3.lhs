> import Control.Monad

Literate Haskell - Ass 3 - Ramon Lopez

Es wird stets der Listenkopf mit dem ersten Element verglichen (sind beides Nullen?)
Falls eine Mehrfachnull entdeckt wird - sofort mit False abbrechen
Falls keine Mehrfachnullfolge entdeckt wird, wird die Liste bis zum Ende durchlaufen

> zeroTest :: [Integer] -> Bool
> zeroTest (x:xs)
>    | xs == [] = True
>    | x == 0 && (head xs) == 0 = False
>    | otherwise = zeroTest xs

___________________________________________
numberOf :: Integer -> Integer

> numberOf :: Integer -> Integer
> numberOf n
>    | n == 0 = 0
>    | n > 0 = toInteger $ length $ filter (\x -> x == True) $ map (zeroTest) (replicateM (fromInteger n) [0,1])


cartProd xs ys = [(x,y) | x <- xs, y <- ys]

__________________________________________
Es wird eine Liste bestehend aus Tupeln generiert. Das Tupel besteht aus 
dem Divisionwert der Münze mit dem Betrag und dem Rest nach der
Division mit dem entsprechenden Münzwert.
Das erste Tupel das passende Münzen enthält (>=1) wird entnommen. Die Funktion wird
mit dem Divisionswert rekursiv aufgerufen. Die Anzahl der passenden Münzen wird in eine Liste
gegeben die ebenfalls übergeben wird.

> minNumOfCoins :: Integer -> Integer
> minNumOfCoins n
>    | n < 0 = -1
>    | otherwise = toInteger $ coins (fromInteger n) []

> euro = [200,100,50,20,10,5,2,1]

> coins :: Int -> [Int] -> Int
> coins a l
>    | a <= 0 = sum l
>    | otherwise = coins (snd (head $ dropWhile (\(a,b) -> a < 1) (zipper a )))  (l ++ [(fst (head $ dropWhile (\(a,b) -> a < 1) (zipper a )))])

> zipper :: Int -> [(Int, Int)]
> zipper a = zip (map (a `div`) euro) (map (a `mod`) euro)

________________________________________
numOfSplits

> numOfSplits :: Integer -> Integer
> numOfSplits n
>    | n <= 500 = toInteger (howManyWays (fromInteger n))
>    | otherwise = -1

> howManyWays a = ways [1,2,5,10,20,50,100,200] !! a
>  where ways [] = 1 : repeat 0
>        ways (coin:coins) =n 
>          where n = zipWith (+) (ways coins) (replicate coin 0 ++ n)

___________________________________

change :: Integer -> Integer -> [Integer]

> change :: Integer -> Integer -> [Integer]
> change n m
>    | n <= 500 = []
>    | otherwise = []



























