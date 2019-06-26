-- Starting code for Assignment 3
-- CISC 260, Winter 2016
-- Emilie Altman 10159164
--
module Assignment3 where

-- Algebraic type for a binary search tree of integers.  A tree may be empty, or it 
-- may have a root, a left subtree and a right subtree.  (Note that one or both of the
-- subtrees might be empty.)  

data Tree = Empty | MakeTree Tree Int Tree

-- Creates a multi-line sideways representation of a tree.  
-- The root will be on the left, with its left child below it 
-- and its right child above it.  If you tilt your head to the
-- left you'll see the structure of the tree.
-- The second parameter is indentation level.
treeToString :: Tree -> Int -> String
treeToString Empty _ = ""
treeToString (MakeTree left root right) indent =
    rightString ++ 
    (spaceString indent) ++ (show root) ++ "\n" ++ 
    leftString
    where
    leftString = treeToString left (indent+4)
    rightString = treeToString right (indent+4)
    
-- Creates a string consisting of n spaces (assuming n >= 0)
spaceString :: Int -> String
spaceString 0 = ""
spaceString n = ' ':(spaceString (n-1))

-- treeToString will be used to display trees
instance Show Tree where show tree = treeToString tree 0

-- The sample tree was given with the assignment
sampleTree = 
    (MakeTree -- tree with root 45
        (MakeTree -- tree with root 15
            (MakeTree -- tree with root 4
                (MakeTree Empty (-1) Empty)
                4
                (MakeTree Empty 7 Empty)
            )
            15
            (MakeTree Empty 23 Empty)
        )
        45
        (MakeTree -- tree with root 72
            Empty
            72
            (MakeTree -- tree with root 103
                (MakeTree Empty 99 Empty)
                103
                Empty
            )
        )
    )

-- Adds a number to a tree, producing a new tree.  No duplicates allowed,
-- so if the number already exists in the tree it is returned without
-- change.
add :: Int -> Tree -> Tree
add x Empty = MakeTree Empty x Empty
add x (MakeTree left root right)
    | x == root = MakeTree left root right -- no duplicates
    | x < root = MakeTree (add x left) root right
    | otherwise = MakeTree left root (add x right)

--find hight of tree
height :: Tree -> Int
height Empty = 0
height (MakeTree left root right) = 1 + max (height left) (height right)

--search tree for number
search :: Int -> Tree -> Bool
search val Empty = False
search val (MakeTree left root right) 
    |val == root = True
    |val < root = search val left 
    |val > root = search val right

--find max value in tree
maxValue :: Tree -> Int
maxValue Empty = error "no max value in empty tree"
maxValue (MakeTree _ root Empty) = root
maxValue (MakeTree left root right) = maxValue right 


--create a tree
createTree :: [Int] -> Tree
createTree list =
    foldr add Empty (reverse list) 


-- Given a sorted list of numbers (with no duplicates) create a balanced or nearly-balanced tree.
balancedTree :: [Int] -> Tree
balancedTree [] = Empty
balancedTree [x] = MakeTree Empty x Empty 
balancedTree x = MakeTree (balancedTree (take center x)) (x!!center) (balancedTree (drop (center+1) x))
    where 
        center = div (length x) 2 


-- Returns a list of the numbers in a tree, from smallest to largest
treeToList :: Tree -> [Int]
treeToList Empty = []
treeToList (MakeTree left root right) = (treeToList left) ++ root : (treeToList right)









