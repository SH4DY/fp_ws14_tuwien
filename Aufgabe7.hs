--Aufgabenblatt 7
import Data.List
import Data.Ord
import Data.Bits

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

vereine = [Sturm,WAC,Austria,WrNeustadt,RBSbg,Groedig,Rapid,Admira,Ried,Altach]

--1. Welche Spieler sind mit den meisten Vereinen Meister geworden?
get_spm :: Historie -> [SpielerId]
get_spm seasons =  sort $ orderByAppearence' $ concat $ map (\ve -> remDup $ concat $ (map (\sa -> was_champ_season ve sa) seasons)) vereine

--2. Welche Vereine haben die Saison am häufigsten auf einem Primzahltabellenplatz abgeschlossen
get_mdhm :: Historie -> [Verein]
get_mdhm hist = sort $ map (fst) $ takeWhile (\(v,p) -> p >= (snd(head allRanks))) allRanks
    where allRanks = reverse $ sortBy (comparing snd) $ map (\v -> (v, countRanksM hist v)) vereine

--3. Welche Vereine haben die Saison am häufigsten auf einem Potenz-von-2 Tabellenplatz abgeschlossen?
get_mdhi :: Historie -> [Verein]
get_mdhi hist = sort $ map (fst) $ takeWhile (\(v,p) -> p >= (snd(head allRanks))) allRanks
    where allRanks = reverse $ sortBy (comparing snd) $ map (\v -> (v, countRanksI hist v)) vereine

--4. Welche Spieler sind am häufigsten Vizemeister geworden, ohne je Meister geworden zu sein?
get_pv :: Historie -> [SpielerId]
get_pv h = reverse $ sort $ filter (\p -> not(elem p champs)) vices
	where vices = sort $ orderByAppearence' $ concat $ map (\v -> concat $ (map (\s -> was_vice_season v s) h)) vereine
	      champs = concat $ map (\v -> concat $ (map (\s -> was_champ_season v s) h)) vereine
--Map über alle Vereine alle saisonen
--verein x wurde in saison y vizemeister?
--füge alle spieler von verein x in liste ein
-- ==> Liste mit allen Spielern die je Vize geworden sind
-- Zählen welcher Spieler am häufigsten in der Liste vorkommt (evt. auch mehrere)

--5. Welche Vereine mit diesem Spieler im Kader haben die Spielzeit am häufigstens auf dem letzten Tabellenplatz beendet?
get_ugr :: Historie -> [SpielerId]
get_ugr h = reverse $ lasts
    where lasts = sort $ orderByAppearence' $ concat $ map (\v -> concat $ (map (\s -> was_last_season v s) h)) vereine

--6. Welche Trainer haben am häufigsten Vereine am Ende der Saison auf einen Stockerlplatz gefuhrt? 
get_tsp :: Historie -> [TrainerId]
get_tsp h = stockerl
    where stockerl = sort $ orderByAppearence' $ concat $ map (\v -> filter (\x -> not(x==TId Z)) (map (\s -> was_stockerl_season v s) h)) vereine

--7. Welche Trainer haben die meisten Vereine am Ende der Saison in die Abstiegszone auf einen der
--drei letzten Tabellenpl¨atze gefuhrt? 
get_taz :: Historie -> [TrainerId]
get_taz h = az
    where az = sort $ orderByAppearence' $ concat $ map (\v -> remDup $ filter (\x -> not(x==TId Z)) (map (\s -> was_az_season v s) h)) vereine

--8. Welche Vereine halten fur ihre Trainer einen Schleudersitz bereit und haben bislan ¨ g auf die meisten
--Trainer vertraut?
get_vsz :: Historie -> [Verein]
get_vsz h = sort $ orderByAppearence' $ map (\x -> fst x) $concat $ map (\v -> (remDup $ map (\(p,k,Tr f)-> (v, f v)) h)) vereine

--9. Welche Trainer halten es nie lange aus bei einem Verein und haben in ihrer Karriere die meisten
--Vereine betreut?
get_tmv :: Historie -> [TrainerId]
get_tmv h = orderByAppearence' $ concat $ map (\v -> (remDup $ map (\(p,k, Tr f) -> f v)h)) vereine

--Helper
get_meister :: Saison -> Verein
get_meister (p, k, t) = fst $ last (sortBy (comparing snd) (map (\x -> (x, p x)) vereine)) 

was_champ_season:: Verein -> Saison -> [SpielerId]
was_champ_season v (p,Kd f,t)
    | (get_meister (p,Kd f,t)) == v = f v
    | otherwise = []

get_vice :: Saison -> Verein
get_vice (p, k, t) = fst $ (xs !! 1)
    where xs = reverse $ (sortBy (comparing snd) (map (\x -> (x, p x)) vereine)) 

was_vice_season:: Verein -> Saison -> [SpielerId]
was_vice_season v (p,Kd f,t)
    | (get_vice (p,Kd f,t)) == v = f v
    | otherwise = []

get_last :: Saison -> Verein
get_last (p, k, t) = fst $ (last xs)
    where xs = reverse $ (sortBy (comparing snd) (map (\x -> (x, p x)) vereine)) 

was_last_season :: Verein -> Saison -> [SpielerId]
was_last_season v (p,Kd f,t)
    | (get_last (p,Kd f,t)) == v = f v
    | otherwise = []

get_placement ::  Saison -> Int -> Verein
get_placement (p,k,t) place
    | place == 1 = get_meister (p,k,t)
    | place > 1 && place <= 10 = fst $ (xs !! (place-1))
    where xs = reverse $ (sortBy (comparing snd) (map (\x -> (x,p x)) vereine)) 

was_stockerl_season :: Verein -> Saison -> TrainerId
was_stockerl_season v (p,k,Tr f)
    | ((get_placement (p,k,Tr f) 1) == v) || ((get_placement (p,k,Tr f) 2) == v) || ((get_placement (p,k,Tr f) 3) == v) = f v
    | otherwise = TId Z

was_az_season :: Verein -> Saison -> TrainerId
was_az_season v (p,k,Tr f)
    | ((get_placement (p,k,Tr f) 8) == v) || ((get_placement (p,k,Tr f) 9) == v) || ((get_placement (p,k,Tr f) 10) == v) = f v
    | otherwise = TId Z

remDup :: Eq a => [a] -> [a]
remDup = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []



orderByAppearence' :: Ord a => [a] -> [a]
orderByAppearence' [] = []
orderByAppearence' (a:as) = map (\(x,y) -> y) (takeWhile (\(x,y) -> (fst $ head (freq)) == x) freq)
    where freq = frequency (a:as)

--Häufigkeit des Vorkommens eines Elements (Rückgabe als Tupel), absteigend sortiert
frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = reverse $ sortBy (comparing fst) (map (\l -> (length l, head l)) (group (sort list)))

--ACHTUNG 0 Indexierung
get_season_rank :: Saison -> Verein -> Maybe Int
get_season_rank s v = elemIndex v (get_season_rankings s) 

get_season_rankings::Saison -> [Verein]
get_season_rankings (p,k,t) = reverse $ map (fst) $ sortBy (comparing snd) $ map (\team -> (team, p team)) vereine

-- Zählt "Mathematiker-Ränge"
countRanksM :: Historie -> Verein -> Int
countRanksM h v = sum $ map (\s -> if (isMdh isPrime $ get_season_rank s v) then 1 else 0) h

--Zählt "Informatiker-Ränge"
countRanksI :: Historie -> Verein -> Int
countRanksI h v = sum $ map (\s -> if (isMdh isPow2 $ get_season_rank s v) then 1 else 0) h

-- +1 weil ein Wert mit 0 Indexierung reinkommt
isMdh :: (Int->Bool) -> Maybe Int -> Bool
isMdh _ Nothing= False
isMdh foo (Just x)
    | not(rank == 0) && not(rank==1) && (foo rank) = True
    | otherwise = False
    where rank = x+1

isPrime :: Int ->Bool
isPrime x = not $ any divisible $ takeWhile notTooBig [2..] where
     divisible y = x `mod`y == 0
     notTooBig y = y*y <= x

isPow2 :: Int -> Bool
isPow2 p
        | p == 0 || p == 1 = False
        | (p .&. (p-1)) == 0 = True
        | otherwise = False

remove_player :: Eq a => a -> [a] -> [a]
remove_player _ [] = []
remove_player x (y:ys)
    | x == y = remove_player x ys
    | otherwise = y : remove_player x ys


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