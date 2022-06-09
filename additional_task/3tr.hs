import System.IO

data Tree a = Leaf | Node (Tree a) a (Tree a)
 
treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node l i r) = i + lh + rh where
    [lh, rh] = map treeSum [l, r]
 
 
treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node l _ r) = 1 + max lh rh where
    [lh, rh] = map treeHeight [l, r]