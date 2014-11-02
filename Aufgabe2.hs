--Aufgabe2 - Fuktionale Programmierung WS14
--Ramon Lopez
import Data.List
import Data.Tuple

--maxWordLength
type Line = String
type Text = [Line]

maxWordLength :: Text -> Integer
maxWordLength a = fromIntegral $ maximum $ map length $ concat $ separateWords a

separateWords :: Text -> [[String]]
separateWords a = map words a

--numWordOcc --TODO

--reverseCheck
reverseCheck::Text -> Bool
reverseCheck a = True `elem` (map compareTuple . zip a $ map reverse a)


compareTuple :: (String,String) -> Bool
compareTuple a = if fst a == snd a then True
	else False

--formatText
formatText :: Text 