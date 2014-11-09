--Aufgabe2 - Fuktionale Programmierung WS14
--Ramon Lopez
import Data.List
import Data.Tuple

--maxWordLength
--Erst werden die einzelnen Worte mittels der words Funktion getrennt
--Dann wird über eine verflachte (concat) Liste gesucht
type Line = String
type Text = [Line]

maxWordLength :: Text -> Integer
maxWordLength a = fromIntegral $ maximum $ map length $ concat $ separateWords a

separateWords :: Text -> [[String]]
separateWords a = map words a


--numWordOcc
--Der Text wird erst in einzelne Wörter aufgeteilt.
--Dann werden die verschachtelten Listen verflacht mit concat (Lines sind hier uninteressant)
--Duplikate von Wörtern entfernen
--Liste von Wörtern filtern die b lang sind
--Ausgabe der Länge der Liste
numWordOcc :: Text -> Integer -> Integer
numWordOcc a b
	| b < 1 = -1
	| otherwise = fromIntegral $ length $ filter (\x -> length x == (fromInteger b)) (removeDuplicates $ concat $ separateWords a)


removeDuplicates :: [String] -> [String]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

--reverseCheck
--Es wird eine Liste von Tupeln gebildet (Zeile, UmgedrehteZeile)
--Dann wird verglichen ob die Elemente der Tupel identisch sind und eine neue Liste erzeugt
--Wenn mindestens 1 Element der neuen Liste True enthält, so gibt die Funktion True zurück
reverseCheck::Text -> Bool
reverseCheck a = True `elem` (map compareTuple . zip a $ map reverse a)


compareTuple :: (String,String) -> Bool
compareTuple a = if fst a == snd a then True
	else False

{- formatText 

formatText :: Text -> Integer -> Text
formatText a i = if length (tail a) <= i then a
	else head a
-}