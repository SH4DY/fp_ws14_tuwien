import Data.List

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

spiel1 = (Sturm, WAC)
spiel2 = (Austria, WrNeustadt)
spiel3 = (RBSbg, Groedig)
spiel4 = (Rapid, Admira)
spiel5 = (Ried, Altach)

spiel6 = (Sturm, Austria)
spiel7 = (RBSbg, Rapid)
spiel8 = (Ried, WAC)
spiel9 = (WrNeustadt, Groedig)
spiel10 = (Admira, Altach)

spiel11 = (WAC, Sturm)
spiel12 = (WrNeustadt, Austria)
spiel13 = (Groedig, RBSbg)
spiel14 = (Admira, Rapid)
spiel15 = (Altach, Ried)

spiel16 = (Ried, Altach)

st1 = St (spiel1, spiel2, spiel3, spiel4, spiel5)
st2 = St (spiel6, spiel7, spiel8, spiel9, spiel10)
st3 = St (spiel11, spiel12, spiel13, spiel14, spiel15)

rp = Rp st1 st2 st3

budget_v :: Verein -> Nat
budget_v v
    | v==Sturm = a1
    | v==WAC = a2
    | v==Austria = a3
    | v==WrNeustadt = a4
    | v==RBSbg = a5
    | v==Groedig = a6
    | v==Rapid = a7
    | v==Admira = a8
    | v==Ried = a9
    | v==Altach = a10

budget_nv :: Verein -> Nat
budget_nv v
    | v==Sturm = a1
    | v==WAC = a2
    | v==Austria = a3
    | v==WrNeustadt = a2
    | v==RBSbg = a5
    | v==Groedig = a6
    | v==Rapid = a7
    | v==Admira = a8
    | v==Ried = a9
    | v==Altach = a10

a1 = (S Z)
a2 = (S (S Z))
a3 = (S (S (S Z)))
a4 = (S (S (S (S Z))))
a5 = (S (S (S (S (S Z)))))
a6 = (S (S (S (S (S (S Z))))))
a7 = (S (S (S (S (S (S (S (S Z))))))))
a8 = (S (S (S (S (S (S (S (S (S Z)))))))))
a9 = (S (S (S (S (S (S (S (S (S (S Z))))))))))
a10 =(S (S (S (S (S (S (S (S (S (S (S Z)))))))))))
--1. Warheitsfunktionen

isValidBudget :: Budget -> Bool
isValidBudget b = (budgetsAreDefined b )&& (budgetsAreDiff b)

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
    | isValidBudget b =  reverse $ map (\(Tm(verein, pkt, bud)) -> verein) $ sort $ map (\x -> (Tm (x, (p x), (b x)))) vereine 
--map (\(verein, pkt, bud) -> verein) $
data Team = Tm (Verein, Punktezahl, Budgethoehe) deriving Show
type Punktezahl = Nat
type Budgethoehe = Nat

instance Eq Team where
 (==) (Tm (v1, p1, b1)) (Tm (v2, p2, b2)) = if(p1 == p2) then (b1 == b2) else (p1 == p2)

instance Ord Team where
 (<) (Tm (v1, p1, b1)) (Tm (v2, p2, b2)) = if(p1 == p2) then (b1 > b2) else (p1 < p2)
 compare (Tm (v1, p1, b1)) (Tm (v2, p2, b2))= if (p1 == p2) then (compare b2 b1) else (compare p1 p2)

team1 = Tm (Sturm, a1, a1)
team2 = Tm (Rapid, a2, a2)
team3 = Tm (Altach, a3, a3)

get_pkt1 :: Verein -> Nat
get_pkt1 v
    | v==Sturm = a10
    | v==WAC = a2
    | v==Austria = a3
    | v==WrNeustadt = a4
    | v==RBSbg = a5
    | v==Groedig = a6
    | v==Rapid = a7
    | v==Admira = a8
    | v==Ried = a9
    | v==Altach = a10
