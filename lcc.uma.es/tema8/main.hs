import Debug.Trace
import Data.List

-- Exercise 8.1 - http://www.lcc.uma.es/~pepeg/declarativa/ejercicios.pdf

splitInMiddle :: [a] -> ([a], [a])
splitInMiddle [] = ([], [])
splitInMiddle list = _split list where
  middle = length list `div` 2

  _split [] = ([], [])
  _split (x:xs) = if length xs <= middle
    then ([x], xs)
    else let (ls, rs) = _split xs in (x:ls, rs)

myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] [] = []
myMerge [] rs = rs
myMerge ls [] = ls
myMerge (l:ls) (r:rs) = if l < r
  then l:myMerge ls (r:rs)
  else r:myMerge (l:ls) rs

mergeSort :: (Ord a, Show a) => [a] -> [a]
mergeSort [] = []
mergeSort [v] = [v]
mergeSort vs =
  let
    (ls, rs) = splitInMiddle vs
  in
    myMerge (mergeSort ls) (mergeSort rs)

-- Exercise 8.2 - http://www.lcc.uma.es/~pepeg/declarativa/ejercicios.pdf

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Exercise 8.3 - http://www.lcc.uma.es/~pepeg/declarativa/ejercicios.pdf

insertInSortedList :: Ord a => [a] -> a -> [a]
insertInSortedList [] v = [v]
insertInSortedList (x:xs) v = if v <= x
  then v:x:xs
  else x:insertInSortedList xs v

customInserSort :: Ord a => [a] -> [a]
customInserSort = foldr (flip insertInSortedList) []

-- Alternative with foldr
-- customInserSort = foldr (\x acc -> insertInSortedList acc x) []

-- First solution
-- customInserSort [] = []
-- customInserSort [x] = [x]
-- customInserSort (x:xs) = buildFromSortedList (x:xs) [] where
--   buildFromSortedList [] l = l
--   buildFromSortedList (x:xs) l = buildFromSortedList xs (insertInSortedList l x)

-- Exercise 8.4 - http://www.lcc.uma.es/~pepeg/declarativa/ejercicios.pdf

inits :: [a] -> [[a]]
inits l = []:foldr (\x acc -> map (x :) ([]:acc)) [] l

tails :: [a] -> [[a]]
tails l = reverse (foldl (\acc x -> map (\y -> y ++ [x]) ([]:acc)) [] l) ++ [[]]

segs :: (Eq a, Ord a) => [a] -> [[a]]
segs [] = [[]]
segs (x:xs) = avoidDuplicateds $ Main.inits (x:xs) ++ segs xs where 
  avoidDuplicateds = map head . group . sort

partes :: (Eq a, Ord a) => [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = avoidDuplicateds $ Main.inits (x:xs) ++ Main.tails (x:xs) ++ tailSegs x (tail xs) ++ segs xs where
  avoidDuplicateds = map head . group . sort
  
  tailSegs :: (Eq a, Ord a) => a -> [a] -> [[a]]
  tailSegs x [] = []
  tailSegs x xs = segs (x:xs) ++ tailSegs x (tail xs)
