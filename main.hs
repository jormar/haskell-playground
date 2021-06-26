import Debug.Trace

addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [ x+y | (x, y) <- xs]

elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem n (x:xs) = n == x || Main.elem n xs


nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs)
  | x `Main.elem` xs = nub xs
  | otherwise = x : nub xs


isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x:y:xs) = (x <= y) && isAsc (y:xs)


hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] a b = False
hasPath graph a b = findPath graph [] [a] b
  where
    findPath :: [(Int, Int)] -> [Int] -> [Int] -> Int -> Bool
    findPath graph visitedNodes fromNodes targetNode | Debug.Trace.trace ("findPath " ++ show graph ++ ". " ++ show visitedNodes ++ ".  (" ++ show fromNodes ++ ", " ++ show targetNode ++ ")") False = undefined
    findPath _ _ [] _ = False
    findPath graph visitedNodes fromNodes targetNode
      | existsExactPath graph fromNodes targetNode = True
      | otherwise = findPath graph allVisitedNodes (middleNodes graph fromNodes allVisitedNodes) targetNode
      where
        allVisitedNodes :: [Int]
        allVisitedNodes = fromNodes ++ visitedNodes

        existsExactPath :: [(Int, Int)] -> [Int] -> Int -> Bool
        existsExactPath graph fromNodes targetNode | Debug.Trace.trace ("    existsExactPath (" ++ show fromNodes ++ ", " ++ show targetNode ++ ")") False = undefined
        existsExactPath graph [] y = False
        existsExactPath graph (x:xs) y = (x,y) `Main.elem` graph || existsExactPath graph xs y

        middleNodes :: [(Int, Int)] -> [Int] -> [Int] -> [Int]
        middleNodes graph fromNodes visitedNodes | Debug.Trace.trace ("    middleNodes " ++ show graph ++ ". " ++ show visitedNodes ++ ".  (" ++ show fromNodes ++ ")") False = undefined
        middleNodes graph [] _ = []
        middleNodes graph (fromNode:xs) visitedNodes =
          [ y | (x,y) <- graph, fromNode==x, not (Main.elem y visitedNodes) ] ++ middleNodes graph xs visitedNodes

hasPath2 :: [(Int, Int)] -> Int -> Int -> Bool
hasPath2 [] x y = x == y
hasPath2 xs x y
  | x == y = True
  | otherwise = let xs' = [ (n,m) | (n,m) <- xs, n /= x] in
    or [ hasPath2 xs' m y | (n,m) <- xs, n == x]

--------------------

rev :: [a] -> [a]
rev = foldl (\acc x -> x:acc) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> map (\y -> x:y) ([]:acc) ) []
