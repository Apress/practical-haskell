module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.Maybe
import Test.Hspec

main :: IO ()
main = defaultMain allTests

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Leaf
                  deriving (Eq, Show)

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r) | x <= y    = Node y (treeInsert x l) r
                          | otherwise = Node y l (treeInsert x r)

treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge t Leaf         = t
treeMerge t (Node x l r) = treeInsert x $ treeMerge (treeMerge t l) r

hunitTestInsertOnLeaf :: TestTree
hunitTestInsertOnLeaf = HU.testCase "Insert 'a' on empty tree" $
  assertEqual "Insertion is wrong" 
              (treeInsert 'a' Leaf) (Node 'a' Leaf Leaf)

allTests :: TestTree
allTests = testGroup "Tasty Tests" [
    testGroup "HUnit Tests" [ hunitTestInsertOnLeaf ]
  ]

main2 = hspec $ do
  describe "Insertion in binary tree" $ do
    it "Inserts correctly 1 in empty tree" $
      True -- treeInsert 1 Leaf @?= Node 1 Leaf Leaf
    it "Finds 1 after inserting it on a tree" $
      isJust $ undefined -- treeFind 1 $ treeInsert 1 (Node 2 Leaf Leaf)
    it "Gets the minimum correctly" $
      pendingWith "Needs to be implemented"
