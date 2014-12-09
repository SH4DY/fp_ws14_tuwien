--Aufgabenblatt 7
import Data.List
import Data.Ord

data Verein = Sturm|WAC|Austria|WrNeustadt|RBSbg|Groedig|Rapid|Admira|Ried|Altach deriving (Eq, Ord, Show)

newtype SpielerId = SId Nat deriving (Eq, Show)
instance Ord SpielerId where
	(<) (SId Z) (SId Z) = False
	(<) (SId x) (SId y) = (x < y)
	--(<) (SId Z) _ = True
	--(<) _ (SId Z) = False
	compare (SId Z) (SId Z) = EQ
	compare (SId Z) (SId x) = LT
	compare (SId x) (SId Z) = GT
	compare (SId x) (SId y) = compare x y
	max x y
	    | x > y            = x
	    | otherwise        = y
	min x y
	    | x < y            = x
	    | otherwise        = y

newtype TrainerId = TId Nat deriving (Eq, Ord, Show)

newtype Kader = Kd (Verein -> [SpielerId])
newtype Trainer = Tr (Verein -> TrainerId)

type Punkte = (Verein -> Nat)

type Saison = (Punkte, Kader, Trainer)

type Historie = [Saison]


--1. Welche Spieler sind mit den meisten Vereinen Meister geworden?
get_spm :: Historie -> [SpielerId]
get_spm seasons =  sort $ orderByAppearence $ concat $ map (\ve -> remDup $ concat $ (map (\sa -> was_champ_season ve sa) seasons)) vereine

--2. Welche Vereine haben die Saison am häufigsten auf einem Primzahltabellenplatz abgeschlossen
get_mdhm :: Historie -> [Verein]
get_mdhm hist = sort $ map (fst) $ takeWhile (\(v,p) -> p >= (snd(head allRanks))) allRanks
    where allRanks = listRevFast $ sortBy (comparing snd) $ map (\v -> (v, countRanks hist v)) vereine
--get_mdhm x = map (countRanks) x

--Map über alle Vereine
--Verein x, map über alle saisonen
--in saison auf mdhm Platz abgeschlossen? +1

--Helper
get_meister :: Saison -> Verein
get_meister (p, k, t) = fst $ last (sortBy (comparing snd) (map (\x -> (x, p x)) vereine)) 

was_champ_season:: Verein -> Saison -> [SpielerId]
was_champ_season v (p,Kd f,t)
    | (get_meister (p,Kd f,t)) == v = f v
    | otherwise = []

remDup = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []
orderByAppearence :: [SpielerId] -> [SpielerId]
orderByAppearence (a:as) = map (\(x,y) -> y) (takeWhile (\(x,y) -> (fst $ head (frequency (a:as))) == x) (frequency (a:as)))

frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = listRevFast $ sortBy (comparing fst) (map (\l -> (length l, head l)) (group (sort list)))

--Schnelles Umkehren von Listen
listRevFast :: [a] -> [a]
listRevFast l = _listRevFast l []
    where
        _listRevFast :: [a] -> [a] -> [a]
        _listRevFast [] l = l
        _listRevFast (x:xs) l = _listRevFast xs (x:l)

--ACHTUNG 0 Indexierung
get_season_rank :: Saison -> Verein -> Maybe Int
get_season_rank s v = elemIndex v (get_season_rankings s) 

get_season_rankings::Saison -> [Verein]
get_season_rankings (p,k,t) = listRevFast $ map (fst) $ sortBy (comparing snd) $ map (\team -> (team, p team)) vereine

--map (countRanks) vereine 
countRanks :: Historie -> Verein -> Int
countRanks h v = sum $ map (\s -> if (isMdhm $ get_season_rank s v) then 1 else 0) h

-- +1 weil ein Wert mit 0 Indexierung reinkommt
isMdhm :: Maybe Int -> Bool
isMdhm Nothing = False
isMdhm (Just x)
    | not(rank == 0) && not(rank==1) && (isPrime rank) = True
    | otherwise = False
    where rank = x+1


isPrime x = not $ any divisible $ takeWhile notTooBig [2..] where
     divisible y = x `mod`y == 0
     notTooBig y = y*y <= x

--Testdaten
vereine = [Sturm,WAC,Austria,WrNeustadt,RBSbg,Groedig,Rapid,Admira,Ried,Altach]
a1 = S (S Z)
a2 = S(S(S(S (S (S ( S( S( S( S Z)))))))))
a3 = S(S(S (S Z)))
a4 = S(S(S(S(S(S(S (S (S ( S( S( S( S Z))))))))))))
a5 = S( S (S Z))

spieler1 = SId a1
spieler2 = SId a2
spieler3 = SId a3
spieler4 = SId a4
spieler5 = SId a5

trainer1 = Tr (get_trainer1)
trainer2 = Tr (get_trainer2)

kader1 = Kd (get_kader1)
kader2 = Kd (get_kader2)

saison_1 = (get_punkte_s1, kader1, trainer1)
saison_2 = (get_punkte_s2, kader2, trainer2)

historie1 = [saison_1, saison_2]
historie2 = [saison_2, saison_1]
--Return Kader

get_kader1 :: Verein -> [SpielerId]
get_kader1 v
    | v == Sturm = [spieler1, spieler2,spieler5]
    | v == Austria = [spieler3, spieler4]

get_kader2 :: Verein -> [SpielerId]
get_kader2 v
    | v == Sturm = [spieler1, spieler2]
    | v == Austria = [spieler1, spieler4, spieler5]

--Return Trainer
get_trainer1 :: Verein -> TrainerId
get_trainer1 v
    | v == Sturm = TId a1
    | v == Austria = TId a2
    | otherwise = TId Z

get_trainer2 :: Verein -> TrainerId
get_trainer2 v
    | v == Sturm = TId a2
    | v == Austria = TId a1
    | otherwise = TId Z

--saisonpunkte
get_punkte_s1 :: Verein -> Nat
get_punkte_s1 v
    | v == Sturm = a2
    | v == Austria = a1
    | otherwise = Z

get_punkte_s2 :: Verein -> Nat
get_punkte_s2 v
    | v == Sturm = a1
    | v == Austria = a2
    | otherwise = Z
--Defintion der Datentypen
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
natToInt :: Nat -> Int
natToInt (S x) = natToInt x + 1
natToInt Z = 0 