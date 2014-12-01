--Ramon Lopez
--Aufgabe 6

import Data.List (maximumBy)
import Data.Function (on)

--DEF
data Nat = Z | S Nat deriving Show

instance Eq Nat where
 (==) Z Z         = True
 (==) (S x) (S y) = (x == y)
 (==) _ _         = False

instance Ord Nat where
 (<) Z Z             = False
 (<) (S x) (S y)     = (x < y)
 (<) Z _             = True
 (<) _ Z             = False
 compare Z Z         = EQ
 compare Z (S x)     = LT
 compare (S x) Z     = GT
 compare (S x) (S y) = compare x y
 max x y
  | x > y            = x
  | otherwise        = y
 min x y
  | x < y            = x
  | otherwise        = y

instance Num Nat where
 (+) Z x         = x
 (+) x Z         = x
 (+) (S x) (S y) = S (S ((+) x y))
 (-) Z _         = Z
 (-) x Z         = x
 (-) (S x) (S y) = (-) x y
 (*) Z _         = Z
 (*) _ Z         = Z
 (*) (S x) (S y) = (S x) + ((S x) * y)
 abs x           = x
 negate _        = Z
 signum x
  | x == Z       = Z
  | otherwise    = (S Z)
 fromInteger x
  | x <= 0       = Z
  | otherwise    = sum (replicate (fromIntegral x) (S Z))
--END DEF

--data Nat = Z | S Nat deriving (Eq, Ord, Show)
type PosRat = (Nat, Nat)
type Skalar = PosRat
type ProtoMatrix = [[Skalar]]
newtype Matrix = M [[Skalar]] deriving Show
type Fuellwert = Integer

-- pm = [[1,2,3],[4,5,6]]


mkMatrix :: ProtoMatrix -> Fuellwert -> Matrix
mkMatrix pm i = M [fill x maxi (intToPosRat (fromIntegral i))|x <- pm]
	where
		maxi = length $ maximumBy (compare `on` length)  pm

fill :: [Skalar] -> Int -> Skalar -> [Skalar]
fill (x:xs) m fuell
    | length (x:xs) >= m = (x:xs)
    | otherwise = fill ((x:xs) ++ [fuell]) m fuell

intToPosRat :: Int -> PosRat
intToPosRat x = (natFromInteger x, natFromInteger 1)

natFromInteger :: Int -> Nat
natFromInteger x
  | x <= 0       = Z
  | otherwise    = sum (replicate (fromIntegral x) (S Z))

a1 = S (S Z)
a2 = S (S (S Z))
posRat = (a2,a1)

proto = [[posRat, posRat],[posRat,posRat,posRat],[posRat]]
