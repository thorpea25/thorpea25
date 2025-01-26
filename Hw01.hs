{--
  CSCI 312 Homework #1

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw01.html
--}

module Hw01 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

type Node = String
type DAG = Map.Map Node (Set.Set Node)

a = "a"
b = "b"
c = "c"
d = "d"
e = "e"

g = Map.fromList [(a, Set.fromList [b,c]),
                  (b, Set.fromList [d]),
                  (c, Set.fromList [d]),
                  (d, Set.fromList []),
                  (e, Set.fromList [a, c])]

-- Put your functions here --------------------
sumUp :: [Int] -> Int
sumUp [] = 0
sumUp (x:xs) = x + (sumUp xs)

evens :: [Int] -> [Int]
evens [] = []
-- evens (x:xs) = [ x | x <- xs, even x ]
evens (x:xs) = if (mod x 2)==0 then x:(evens xs) else (evens xs)

incAll :: [Int] -> [Int]
incAll [] = []
incAll (x:xs) = x+1 : incAll xs

incBy :: Int -> [Int] -> [Int]
incBy n [] = []
incBy n (x:xs) = x+n : incBy n xs


append :: [Int] -> [Int] -> [Int]
append xs [] = xs
append [] ys = ys
append (x:xs) ys = x : (append xs ys)

data IntTree = Empty | Node IntTree Int IntTree deriving (Eq,Show)
isLeaf :: IntTree -> Bool
isLeaf Empty = True
isLeaf (Node l x r) = 
  if l == Empty && r == Empty then True
  else if l == Empty || r == Empty then False
  else isLeaf l && isLeaf r

sumTree :: IntTree -> Int
sumTree Empty = 0
sumTree (Node l x r) = 
  if l == Empty then x + sumTree r
  else if r == Empty then x + sumTree l
  else x + sumTree l + sumTree r

fringe :: IntTree -> [Int]
fringe (Node l x r) =
      if isLeaf (Node l x r) then [x]
      else if isLeaf l then [x]
      else if isLeaf r then  [x]
      else fringe l ++ fringe r

-- cs.pomona.edu
isBST :: IntTree -> Bool
isBST t = isBST' Nothing Nothing t
 where isBST' lower upper Empty = True
       isBST' lower upper (Node l x r) =
           maybeBounded lower upper x &&
           isBST' lower (Just x) l &&
           isBST' (Just x) upper r
       maybeBounded Nothing Nothing x = True
       maybeBounded Nothing (Just upper) x = x < upper
       maybeBounded (Just lower) Nothing x = lower < x
       maybeBounded (Just lower) (Just upper) x = lower < x && x < upper

sumUp' :: [Int] -> Int
sumUp' [] = 0
sumUp' l = foldr (+) 0 l


evens' :: [Int] -> [Int]
evens' [] = []
evens' (x:xs) = foldr (++) [] [[x] | x <- xs, even x] 

incAll' :: [Int] -> [Int]
incAll' [] = []
incAll' l = map (+1) l

incBy' :: Int -> [Int] -> [Int]
incBy' n l = map (+n) l

map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (x:xs) = f x : map1 f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 p (x:xs)
      | p x = x : filter1 p xs
      | otherwise = filter1 p xs

sqrt' :: Float -> Maybe Float
sqrt' x = 
  if x >= 0 then Just (sqrt x) 
  else Nothing

div' :: Float -> Float -> Either String Float
div' x 0 = Left "Can't divide by 0!"
div' x y = Right (x/y)

swap :: (a,b) -> (b,a)
swap (x, y) = (y,x)

pairUp :: [a] -> [b] -> [(a,b)]
pairUp [] [] = []
pairUp (x:xs) (y:ys) = [(x, y)] ++ pairUp (xs) (ys)

splitUp :: [(a,b)] -> ([a],[b])
splitUp [] = ([],[])
splitUp xs = (map fst xs, map snd xs)

sumAndLength :: [Int] -> (Int,Int)
sumAndLength [] = (0, 0)
sumAndLength (x:xs) = (a, b) where 
  a = x + fst(sumAndLength xs)
  b = snd(sumAndLength xs) + 1


hasPath :: DAG -> Node -> Node -> Bool 
hasPath g x y = if y `Set.member` (neighbors x) then True else if any' (Set.map (hasPath g y ) (neighbors(x))) then True else False



neighbors :: Node -> Set.Set Node
neighbors n = case Map.lookup n g of
  Nothing -> error "Map.find: element not in the map"
  Just x -> x

any' :: (Set.Set Bool) -> Bool
any' a = if (Set.member True a) then True else False


-- Tests ----------------------------------------

main = do

    putStrLn "Problem 1: natural recursion -----------------------------------\n"

    putStr "Should be 6: "
    print $ sumUp [1,2,3]

    putStr "Should be [2,4,6,8]: "
    print $ evens [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incAll [1,2,3,4,5,6,7,8,9]

    putStr "Should be 3,4,5,6,7,8,9,10,11]: "
    print $ incBy 2 [1,2,3,4,5,6,7,8,9]

    putStr "Should be 1,2,3]: "
    print $ append [] [1,2,3]

    putStr "Should be [1,2,3]: "
    print $ append [1,2,3] []

    putStr "Should be [1,2,3,4,5,6]: "
    print $ append [1,2,3] [4,5,6]

    putStrLn "\nProblem 2: data types -----------------------------------------\n"

    putStr "Should be True: "
    print $ isLeaf Empty

    putStr "Should be True: "
    print $ isLeaf (Node Empty 3 Empty)

    putStr "Should be False: "
    print $ isLeaf (Node (Node Empty 1 Empty) 2 Empty)

    putStr "Should be 10: "
    print $ sumTree (Node (Node Empty 1 Empty) 3 (Node Empty 2 (Node Empty 4 Empty)))

    putStr "Should be [2,7]: "
    print $ fringe (Node (Node Empty 1 (Node Empty 2 Empty))
                          5
                          (Node (Node Empty 7 Empty) 10 Empty))

    putStrLn "\nProblem 3: binary search trees --------------------------------\n"

    putStr "Should be True: "
    print $ isBST (Node (Node Empty 2 Empty)  4 (Node Empty 5 Empty))

    putStr "Should be False: "
    print $ isBST (Node (Node Empty 5 Empty)  4 (Node Empty 2 Empty))

    putStrLn "\nProblem 4: map and filter -------------------------------------\n"

    putStr "Should be 6: "
    print $ sumUp' [1,2,3]

    putStr "Should be [2,4,6,8]: "
    print $ evens' [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incAll' [1,2,3,4,5,6,7,8,9]

    putStr "Should be 3,4,5,6,7,8,9,10,11]: "
    print $ incBy' 2 [1,2,3,4,5,6,7,8,9]

    putStrLn "\nProblem 5: defining higher-order functions --------------------\n"

    putStr "Should be [1,4,9,16,25]: "
    print $ map1 (\x -> x * x) [1,2,3,4,5]

    putStr "Should be [1,3,5,7,9]: "
    print $ filter1 odd [0,1,2,3,4,5,6,7,8,9]

    putStrLn "\nProblem 6: Maybe and Either ------------------------------------\n"

    putStr "Should be [0.0,1.0,2.0,3.0]: "
    print $ mapMaybe sqrt' [0,-1,1,-4,4,9,-9]

    putStrLn "\nProblem 7: Creating polymorphic data types ---------------------\n"

    putStr "Should be (\"hello\", 3): "
    print $ swap (3, "hello") 

    putStr "Should be [(0,1),(2,3),(4,5),(6,7),(8,9)]: "
    print $ pairUp [0,2,4,6,8] [1,3,5,7,9]

    putStr "Should be ([0,2,4,6,8],[1,3,5,7,9]): "
    print $ splitUp [(0,1),(2,3),(4,5),(6,7),(8,9)]

    putStr "Should be (15, 5): "
    print $ sumAndLength [1,2,3,4,5]

    case div' 1 0 of
      Right val -> print $ val
      Left  msg -> putStrLn msg

    case div' 1 2 of
      Right val -> print $ val
      Left  msg -> putStrLn msg

    putStrLn "\nProblem 8: maps and sets --------------------------------------\n"

    putStr "Should be True: "
    print $ hasPath g a d

    putStr "Should be False: "
    print $ hasPath g a e

