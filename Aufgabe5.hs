data Tree = Node (Label -> Label) Label [Tree]
type Label = Int

tcm :: (Label -> Bool) -> (Label -> Label) -> Tree -> Tree
tcm con trans (Node trans' l xs)
    | con l = (Node trans' (trans l) (map (tcm con trans) xs))
    | otherwise = (Node trans' (trans' l) (map (tcm con trans) xs))

tzp :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
tzp f (Node f1 l1 xs)(Node f2 l2 ys)
    | l1 == l2 = Node (f l1) (f2(f1 l1)) (zipWith (tzp f) xs ys)
    | otherwise = Node (f1 . f2) (f l1 l2) (zipWith (tzp f) xs ys)

data STree = SNode Label [STree] deriving (Eq, Show)

t2st :: Tree -> STree
t2st (Node f l xs) = (SNode l (map t2st xs))

