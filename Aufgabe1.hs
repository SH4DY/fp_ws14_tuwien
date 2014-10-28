import Data.Char
import Data.Bits

--Ramon Lopez Narvaez - 1328066

--1. tightPrimeEmbedding
tightPrimeEmbedding :: Integer -> (Integer, Integer, Integer)
tightPrimeEmbedding n
        | n >= 2 = (fromIntegral (primeSmallerOrEqualThan (fromIntegral n)), n , fromIntegral (primeBiggerThan (fromIntegral n)))
        | otherwise = (0,n,0)

--Gibt alle Primzahlen bis zu einer Obergrenze aus        
take_primes n = take n $ sieb[2..]

sieb :: [Int] -> [Int]
sieb (l:ls) = l:sieb[x | x <- ls, mod x l /= 0]

--Gibt q aus. Vorgabe p <= n < q
primeBiggerThan :: Int -> Int
primeBiggerThan n =  minimum (dropWhile (<=n) (take_primes n))

--Gibt p aus. Vorgabe p <= n < q
primeSmallerOrEqualThan :: Int -> Int
primeSmallerOrEqualThan n =  maximum (takeWhile (<=n) (take_primes n))

--2. checksum
checksum :: Integer -> String
checksum x
	| x == 0 = "0"
	| otherwise = myToString (toBin( quersumme (fromIntegral x))) --TODO

--Bilder quersumme einer gegebenen int Zahl
quersumme :: Int -> Int
quersumme x
	| x == 0 = 0
	| otherwise = x `mod`10 + quersumme (x `div` 10)

--Wandelt gegebenes n ins Dualsystem
toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]


myToString :: [Int] -> String
myToString x = map intToDigit x

--3. filterForFrequency
filterForFrequency :: String -> Int -> [Char]
filterForFrequency [] _ = ""
filterForFrequency (x:xs) i = if countOccurence x (x:xs) == i then [x]++(filterForFrequency xs i) 
	else filterForFrequency xs i 

countOccurence :: Eq a => a -> [a] -> Int
countOccurence _ [] = 0
countOccurence b (a:as) | b == a = 1+(countOccurence b as)
                        | otherwise = countOccurence b as

--4. isPowerOfTwo

isPowerOfTwo :: String -> Bool
isPowerOfTwo x = isPowerOfTwoInBit((bigVocals x) * (smallVocals x))

isPowerOfTwoInBit :: Int -> Bool
isPowerOfTwoInBit b
        | b == 0 || b == 1 = False
        | (b .&. (b-1)) == 0 = True
        | otherwise = False

bigVocals :: String -> Int
bigVocals "" = 0
bigVocals (x:xs) = if isVocalG x then (bigVocals (removeElement x xs)) + ((countOccurence x xs)+1)
	else bigVocals xs

smallVocals :: String -> Int
smallVocals "" = 0
smallVocals (x:xs) = if isVocalK x then (smallVocals (removeElement x xs)) + ((countOccurence x xs)+1)
	else smallVocals xs

-- Already defined for exercise 2
{- countOccurence :: Eq a => a -> [a] -> Int
countOccurence _ [] = 0
countOccurence b (a:as) | b == a = 1+(countOccurence b as)
                        | otherwise = countOccurence b as
-}

isVocalG :: Char -> Bool
isVocalG a = if a == 'A'|| a == 'E'|| a == 'I'|| a =='O'|| a == 'U' then True
	else False

isVocalK :: Char -> Bool
isVocalK a = if a == 'a'|| a == 'e'|| a == 'i'|| a =='o'|| a == 'u' then True
	else False


removeElement y xs = filter (/= y) xs
