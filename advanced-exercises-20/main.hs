import Debug.Trace
import Data.List ( sort )
-- import Data.Tuple

-- len :: [a] -> Int
-- len = foldr (\x -> (+) 1) 0

-- lagrange :: [(Float, Float)] -> Float -> Float
-- lagrange points x = snd (foldr (\vj (j, acc) -> (j + 1, (snd vj * l j vj x points) + acc)) (0, 0) points)
--   where
--     l j vj x points = snd (foldr cociente (0, 0) points)
--       where
--         cociente vm (m, acc)
--           | j /= m = (m + 1, (x - fst vm) / (fst vj - fst vm))
--           | otherwise = (m + 1, acc)


-- data Trie a = Leaf a | Node a [Trie a]

-- foldtrie :: (b -> a -> b) -> b  -> Trie a -> b
-- foldtrie f acc (Leaf a) = f acc a
-- foldtrie f acc (Node a xs) = foldl (foldtrie f) (f acc a) xs

nat = asc 1
  where asc n = n : asc (n + 1)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving( Show )

invTupTree :: Tree (Integer , Integer)
invTupTree = invTupTreeSucc (0,0)
  where
    invTupTreeSucc :: (Integer , Integer) -> Tree (Integer , Integer)
    invTupTreeSucc (l, r) = Node (invTupTreeSucc (l+1, r)) (l, r) (invTupTreeSucc (l, r+1))

cut :: Integer -> Tree a -> Tree a
-- cut n (Node lt (x, y) rt) | trace ("CUT " ++ show (x, y)) False = undefined
cut 0 _ = Leaf
cut n Leaf = Leaf
cut n (Node lt v rt) = Node (cut (n-1) lt) v (cut (n-1) rt)

insert :: Ord a => a -> Tree a -> Tree a
insert a Leaf = Node Leaf a Leaf
insert a (Node l v r)
  | a <= v = Node (insert a l) v r
  | otherwise  = Node l v (insert a r)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r

prop_IIS xs = Data.List.sort xs == xs'
  where
    types = xs :: [Int]
    xs' = inorder $ foldr insert Leaf xs
