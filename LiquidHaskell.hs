{-@ LIQUID "--no-termination" @-}
module LiquidHaskell where

import Data.Set

data Tree a = Empty | Node (Tree a) a (Tree a)
             deriving (Show, Eq, Ord)

{-@ measure treeSize @-}
{-@ treeSize :: Tree a -> Nat @-}
treeSize :: Tree a -> Int
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

{-@ measure treeElements @-}
treeElements :: (Ord a) => Tree a -> Set a
treeElements Empty        = empty
treeElements (Node l x r) = singleton x `union` treeElements l `union` treeElements r

{-@ treeInsert :: x: a -> v: Tree a -> {w: Tree a | treeSize w = treeSize v + 1 && member x (treeElements w) } @-}
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Empty = Node Empty x Empty
treeInsert x (Node l y r)
  | x <= y    = Node (treeInsert x l) y r  -- for example, if I forger to do the insertion recursively
  | otherwise = Node l y (treeInsert x r)  -- for example, if I write y instead of x I get an error

{-
 /home/serras/beginning-haskell-chapter12/LiquidHaskell.hs:24:17-26: Error: Liquid Type Mismatch
  
 24 |   | x <= y    = Node l y r
                      ^^^^^^^^^^
  
   Inferred type
     VV : {v : (Tree a) | LiquidHaskell.treeSize v == (1 + LiquidHaskell.treeSize l) + LiquidHaskell.treeSize r
                          && LiquidHaskell.treeElements v == Set_cup (Set_cup (Set_sng y) (LiquidHaskell.treeElements l)) (LiquidHaskell.treeElements r)
                          && LiquidHaskell.treeSize v >= 0}
  
   not a subtype of Required type
     VV : {VV : (Tree a) | LiquidHaskell.treeSize VV == LiquidHaskell.treeSize ?a + 1
                           && Set_mem x (LiquidHaskell.treeElements VV)}
-}

{-
 /home/serras/beginning-haskell-chapter12/LiquidHaskell.hs:25:17-41: Error: Liquid Type Mismatch
  
 25 |   | otherwise = Node l y (treeInsert y r)
                      ^^^^^^^^^^^^^^^^^^^^^^^^^
  
   Inferred type
     VV : {v : (Tree a) | LiquidHaskell.treeSize v == (1 + LiquidHaskell.treeSize l) + LiquidHaskell.treeSize ?b
                          && LiquidHaskell.treeElements v == Set_cup (Set_cup (Set_sng y) (LiquidHaskell.treeElements l)) (LiquidHaskell.treeElements ?b)
                          && LiquidHaskell.treeSize v >= 0}
  
   not a subtype of Required type
     VV : {VV : (Tree a) | LiquidHaskell.treeSize VV == LiquidHaskell.treeSize ?a + 1
                           && Set_mem x (LiquidHaskell.treeElements VV)}
-}

data SearchTree a = EmptyS | NodeS a (SearchTree a) (SearchTree a)
                  deriving (Show, Eq, Ord)

{-@ data SearchTree a = EmptyS
                      | NodeS { x:: a
                              , left :: SearchTree {v: a | v <= x}
                              , right :: SearchTree {v: a | v > x} }
@-}

{-
wrong :: a -> SearchTree a -> SearchTree a -> SearchTree a
wrong x t1 t2 = NodeS x t1 t2
-}
{-
/home/serras/beginning-haskell-chapter12/LiquidHaskell.hs:69:17-26: Error: Liquid Type Mismatch
  
 69 | wrong x t1 t2 = NodeS x t1 t2
                      ^^^^^^^^^^
  
   Inferred type
     VV : a
  
   not a subtype of Required type
     VV : {VV : a | VV <= x}
  
   In Context
     x : a


 /home/serras/beginning-haskell-chapter12/LiquidHaskell.hs:69:17-29: Error: Liquid Type Mismatch
  
 69 | wrong x t1 t2 = NodeS x t1 t2
                      ^^^^^^^^^^^^^
  
   Inferred type
     VV : a
  
   not a subtype of Required type
     VV : {VV : a | VV > x}
  
   In Context
     x : a
-}

treeInsertS :: Ord a => a -> SearchTree a -> SearchTree a
treeInsertS x EmptyS = NodeS x EmptyS EmptyS
treeInsertS x (NodeS y l r)
  | x <= y    = NodeS y (treeInsertS x l) r
  | otherwise = NodeS y l (treeInsertS x r)

{-
/home/serras/beginning-haskell-chapter12/LiquidHaskell.hs:106:26-40: Error: Liquid Type Mismatch
  
 106 |   | x <= y    = NodeS x (treeInsertS y l) r  -- if I mistake, I get an error
                                ^^^^^^^^^^^^^^^
  
   Inferred type
     VV : a
  
   not a subtype of Required type
     VV : {VV : a | VV <= x}
  
   In Context
     x : a
-}

-- Try to write a merge function, on both binary trees and search trees
-- Think about properties such as: how does the elements and size look like?