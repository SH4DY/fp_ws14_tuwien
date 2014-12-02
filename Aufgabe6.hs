--Ramon Lopez
--Aufgabe 6

import Data.List (maximumBy)
import Data.Function (on)
import Numeric

--DEF
data Nat = Z | S Nat deriving (Show)

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
newtype Matrix = M [[Skalar]]
type Fuellwert = Integer



--Aufgabe 6.1
mkMatrix :: ProtoMatrix -> Fuellwert -> Matrix
mkMatrix pm i = M [fill x maxi (intToPosRat (fromIntegral i))|x <- pm]
	where
		maxi = length $ maximumBy (compare `on` length)  pm

fill :: [Skalar] -> Int -> Skalar -> [Skalar]
fill (x:xs) m fuell
    | length (x:xs) >= m = (x:xs)
    | otherwise = fill ((x:xs) ++ [fuell]) m fuell

intToPosRat :: Int -> PosRat
intToPosRat x 
    | x == 0 = (Z, Z)
    | otherwise = (natFromInteger x, natFromInteger 1)

natFromInteger :: Int -> Nat
natFromInteger x
  | x <= 0       = Z
  | otherwise    = sum (replicate (fromIntegral x) (S Z))

--Aufgabe6.2
data OktoZiffern = E | Zw | D | V | F | Se | Si | N
type OktoZahlen = [OktoZiffern]


--instance Show Matrix where
--	show (M x) = ()

--posRatToOct :: PosRat -> String
--posRatToOct (x,y) = "(" ++ (showOct x "") ++ "/" ++ (showOct y "") ++ ")"

--Aufgabe3

instance Eq Matrix where
    (==) (M m1) (M m2) = (length m1) == (length m2) && (all (\x -> length x == dim) m1) && (all (\x -> length x == dim) m2) && (all (\(x,y) -> x==y)(zip m1 m2))
	    where dim = (length (m1 !! 1))

--Aufgabe4
data OrderingMat = EQM | LTM | GTM | INC deriving (Eq, Show)

class (Eq a) => OrdMat a where
    lsm, lem, grm, gem :: a -> a -> Bool
    cmpm :: a -> a -> OrderingMat

instance OrdMat Matrix where
    lsm (M m1) (M m2) = (compareDim (M m1) (M m2)) && (all (\(x,y) -> x < y)(zip m1 m2))
    lem (M m1) (M m2) = (compareDim (M m1) (M m2)) && (all (\(x,y) -> x <= y)(zip m1 m2))
    grm (M m1) (M m2) = (compareDim (M m1) (M m2)) && (all (\(x,y) -> x > y)(zip m1 m2))
    gem (M m1) (M m2) = (compareDim (M m1) (M m2)) && (all (\(x,y) -> x >= y)(zip m1 m2))
    cmpm x y
        | x == y = EQM
        | lsm x y = LTM
        | grm x y = GTM
        | otherwise = INC

compareDim :: Matrix -> Matrix -> Bool
compareDim (M m1) (M m2) = (length m1) == (length m2) && (all (\x -> length x == dim) m1) && (all (\x -> length x == dim) m2)
    where dim = (length (m1 !! 1))

--Testdaten
a1 = S (S Z)
a2 = S (S (S Z))
posRat = (a2,a1) --1,5
posRat2 = (a1, a2) --0,6

proto = [[posRat, posRat],[posRat,posRat,posRat],[posRat]]

mat1 = M [[posRat, posRat],[posRat,posRat],[posRat, posRat]]
mat2 = M [[posRat2, posRat2],[posRat2,posRat2],[posRat2, posRat2]]
