--Ramon Lopez Narvaez
--Aufgabe 5 - Funktionale auf algebraischen Datentypen


data Tree = Node (Label -> Label) Label [Tree]
type Label = Int

--Falls die Markierung die Konvertierungsfunktion so wird sie
--mit der übergebenen Funktion umgewandelt (1. Fall), ansonsten mit
--der trans'-Funktion des Knotens (otherwhise Fall). Schlussendlich wird 
--tcm rekursiv auf alle Kinder angewandt.
tcm :: (Label -> Bool) -> (Label -> Label) -> Tree -> Tree
tcm con trans (Node trans' l xs)
    | con l = (Node trans' (trans l) (map (tcm con trans) xs))
    | otherwise = (Node trans' (trans' l) (map (tcm con trans) xs))

--Übergabe der Transformationsfunktion oder Sequentielle
--Ausführung falls die Markierungen nicht übereinstimmen.
--Anschließend wird mittels zipWith tzp auf den Kind-Knoten aufgerufen  
tzp :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
tzp f (Node f1 l1 xs)(Node f2 l2 ys)
    | l1 == l2 = Node (f l1) (f2(f1 l1)) (zipWith (tzp f) xs ys)
    | otherwise = Node (f1 . f2) (f l1 l2) (zipWith (tzp f) xs ys)

data STree = SNode Label [STree] deriving (Eq, Show)

--Transformation der komplexeren Baumstruktur in die
--einfache
t2st :: Tree -> STree
t2st (Node f l xs) = (SNode l (map t2st xs))

--Summieren der Labelwerte mittels Rekursion
--auf allen Kindern.
tsum :: STree -> Label
tsum (SNode l xs)
    | xs == [] = l
    | otherwise = sum (map tsum xs) + l

--Tiefenzählung
tdepth :: STree -> Label
tdepth tree
    | (tdepth' tree) == 1 = 1
    | otherwise = (tdepth'› tree) -1

tdepth' :: STree -> Label
tdepth' (SNode l xs)
    | xs == [] = 1
    | otherwise = sum (map tdepth' xs) + 1


