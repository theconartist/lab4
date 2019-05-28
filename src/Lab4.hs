--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 4: User-defined types                                                  --
--------------------------------------------------------------------------------

module Lab4 where

--------------------------------------------------------------------------------
-- Red-black trees

data Colour = Red | Black

instance Show Colour where
    show Red = "Red"
    show Black = "Black"

data Tree a = Leaf | Node Colour (Tree a) a (Tree a)
    deriving Show

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton x = Node Red Leaf x Leaf

makeBlack :: Tree a -> Tree a
makeBlack (Node c l x r) = Node Black l x r

depth :: Tree a -> Int
depth Leaf = 0
depth (Node c l x r) = max (depth l) (depth r) + 1

toList :: Tree a -> [a]
toList Leaf = []
toList (Node c l x r) = toList l ++ [x] ++ toList r

member :: Ord a => a -> Tree a -> Bool
member x t = x `elem` toList t

balance :: Colour -> Tree a -> a -> Tree a -> Tree a
balance Black (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance _ _ _ _ = undefined
balance _ _ _ _ = undefined
balance _ _ _ _ = undefined
balance c l x r = Node c l x r

insert :: Ord a => Tree a -> a -> Tree a
insert = undefined

--------------------------------------------------------------------------------
