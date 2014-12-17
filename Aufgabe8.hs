module Aufgabe8 where


import Data.List
import Data.Ord

data Verein = Sturm | WAC | Austria | WrNeustadt | RBSbg| Groedig | Rapid | Admira | Ried | Altach deriving (Eq,Ord,Show)

type Spiel = (Verein,Verein)

newtype Spieltag = St (Spiel,Spiel,Spiel,Spiel,Spiel) deriving (Eq,Show)

data Restprogramm = Rp Spieltag Spieltag Spieltag

type Punkte = (Verein -> Nat)

type Budget = (Verein -> Nat)

data Nat = Z | S Nat deriving (Eq, Ord, Show)

data Herbstmeister = NochOffen | AlsHMstehtfest Verein deriving (Eq, Show)

--Sieg = 3, Niederlage 0, Unentschied 1 für beide
--Wert "Punkte" = punktestand nach viertletztem Spieltag
--Bei Punktgleichheit = niedrigeres Budget bedeutet besserer Platz
vereine = [Sturm, WAC, Austria, WrNeustadt, RBSbg, Groedig, Rapid, Admira, Ried, Altach]
--Testdaten



--1. Warheitsfunktionen

isValidBudget :: Budget -> Bool
isValidBudget b = (budgetsAreDefined b )&& (budgetsAreDiff b)

--"...können davon ausgehen, dass alle Budgets definiert sind."
budgetsAreDefined b = True

budgetsAreDiff :: Budget -> Bool
budgetsAreDiff b = if (length (nub $ map (\v -> b v) vereine)) == 10 then True else False

isValidSpiel :: Spiel -> Bool
isValidSpiel (x,y) = if(x == y) then False else True 

isValidSpieltag :: Spieltag -> Bool
isValidSpieltag st = (allSpieleValid st) && (allVereineSet st)

--Jeder Verein kommt genau 1 mal vor
allVereineSet ::Spieltag -> Bool
allVereineSet (St (sp1, sp2, sp3, sp4, sp5)) = if(length $ nub $ listVereineOfSpieltag (St (sp1, sp2, sp3, sp4, sp5))) == 10 then True else False

allSpieleValid :: Spieltag -> Bool
allSpieleValid (St (sp1, sp2, sp3, sp4, sp5)) = (isValidSpiel sp1) && (isValidSpiel sp2) && (isValidSpiel sp3) && (isValidSpiel sp4) && (isValidSpiel sp5)

--Alle 3 Spieltage müssen gültig sein && alle Spiele sind verschieden
isValidRestprogramm :: Restprogramm -> Bool
isValidRestprogramm (Rp stag1 stag2 stag3) = (isValidSpieltag stag1)&&(isValidSpieltag stag2)&&(isValidSpieltag stag3) && ((length $ nub ((listVereineOfSpieltagPair stag1) ++ (listVereineOfSpieltagPair stag2) ++ (listVereineOfSpieltagPair stag3))) == 15)

listVereineOfSpieltag :: Spieltag -> [Verein]
listVereineOfSpieltag (St (sp1, sp2, sp3, sp4, sp5)) =  ([fst sp1, snd sp1] ++ [fst sp2, snd sp2] ++ [fst sp3, snd sp3] ++ [fst sp4, snd sp4] ++ [fst sp5, snd sp5])

listVereineOfSpieltagPair :: Spieltag -> [Spiel]
listVereineOfSpieltagPair (St (sp1, sp2, sp3, sp4, sp5)) = [sp1,sp2,sp3,sp4,sp5]

--2 Punktestand gemäss Punkte und Budget
--Budget checken -> Panikmodus Fehlermeldung "Ungueltige Eingabe"
mkTabelle :: Punkte -> Budget -> [Verein]
mkTabelle p b
    | not(isValidBudget b) = error "Ungueltige Eingabe" 
    | otherwise =  reverse $ map (\(Tm(verein, pkt, bud)) -> verein) $ sort $ map (\x -> (Tm (x, (p x), (b x)))) vereine

--3 Herbstmeister

hm_fix :: Punkte -> Budget -> Restprogramm -> Herbstmeister
hm_fix p b (Rp stag1 stag2 stag3)
    | not(isValidBudget b) || not(isValidRestprogramm (Rp stag1 stag2 stag3)) = error "Ungueltige Eingabe"
    | (head $ leaderboard p b) > ((addWinsToTeam((leaderboard p b) !! 1))) = AlsHMstehtfest ((\ (Tm(vr,pkt,bud)) -> vr) (head $ leaderboard p b))
    | otherwise = NochOffen

hm_alleMitRechnerischerChance :: Punkte -> Budget -> Restprogramm -> [Verein]
hm_alleMitRechnerischerChance p b (Rp stag1 stag2 stag3)
    | not(isValidBudget b) || not(isValidRestprogramm (Rp stag1 stag2 stag3)) = error "Ungueltige Eingabe"
    | otherwise = map (\(vr, nat) -> vr) $ sortBy (comparing snd) $ map (\(vr, bl) -> (vr, (b vr))) $ filter (\(vr, bl) -> bl == True) $ map (\v -> (v, (potentialLeader (Tm (v, p v, b v)) p b) )) vereine

vhm_alleMitRechnerischerChance :: Punkte -> Budget -> Restprogramm -> [Verein]
vhm_alleMitRechnerischerChance p b (Rp stag1 stag2 stag3)
    | not(isValidBudget b) || not(isValidRestprogramm (Rp stag1 stag2 stag3)) = error "Ungueltige Eingabe"
    | otherwise = map (\(vr, nat) -> vr) $ sortBy (comparing snd) $ map (\(vr, bl) -> (vr, (b vr))) $ filter (\(vr, bl) -> bl == True) $ map (\v -> (v, (potentialLeader (Tm (v, p v, b v)) p b) )) vereine

hm_ausEigenerKraft :: Punkte -> Budget -> Restprogramm -> [Verein]
hm_ausEigenerKraft p b (Rp stag1 stag2 stag3)
    | not(isValidBudget b) || not(isValidRestprogramm (Rp stag1 stag2 stag3)) = error "Ungueltige Eingabe"
    | otherwise= [((mkTabelle p b) !! 0)]

leaderboard :: Punkte -> Budget -> [Team]
leaderboard p b = reverse $ sort $ map (\x -> (Tm (x, (p x), (b x)))) vereine

addWinsToTeam :: Team -> Team
addWinsToTeam (Tm (v,p,b)) = (Tm (v,p + fromInteger(9),b))

potentialLeader :: Team -> Punkte -> Budget-> Bool 
potentialLeader (Tm(v,p,b)) pf bf
    | (head $ leaderboard pf bf) == (Tm(v,p,b)) = True 
    | otherwise = if ((addWinsToTeam (Tm(v, p,b))) > (head $ leaderboard pf bf)) then True else False

potentialViceLeader :: Team -> Punkte -> Budget-> Bool 
potentialViceLeader (Tm(v,p,b)) pf bf
    | ((head $ leaderboard pf bf) == (Tm(v,p,b))) || (((leaderboard pf bf) !! 1) == (Tm(v,p,b)))  = True 
    | otherwise = if ((addWinsToTeam (Tm(v, p,b))) > (leaderboard pf bf) !! 1 ) then True else False


data Team = Tm (Verein, Punktezahl, Budgethoehe) deriving Show
type Punktezahl = Nat
type Budgethoehe = Nat

instance Eq Team where
 (==) (Tm (v1, p1, b1)) (Tm (v2, p2, b2)) = if(p1 == p2) then (b1 == b2) else (p1 == p2)

instance Ord Team where
 (<) (Tm (v1, p1, b1)) (Tm (v2, p2, b2)) = if(p1 == p2) then (b1 > b2) else (p1 < p2)
 compare (Tm (v1, p1, b1)) (Tm (v2, p2, b2))= if (p1 == p2) then (compare b2 b1) else (compare p1 p2)


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
