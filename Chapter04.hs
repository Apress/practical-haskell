{-# LANGUAGE RecordWildCards #-}

module Chapter04 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree
import Data.Graph

m1 = M.singleton "hello" 3
m2 = M.fromList [("hello",1),("bye",2),("hello",3)]
m3 = let m1' = M.singleton "hello" 3
         m2' = M.insert "bye" 2 m1'
         m3' = M.insert "hello" 5 m2'
         m4' = M.insertWith (+) "hello" 7 m3'
     in (m1',m2',m3',m4')
m4 = M.fromList [("hello",3),("bye",4)]
m5 = M.adjust (+7) "hello" m4
m6 = M.alter (\(Just v) -> Just (v+7)) "hello" m4
m7 = let m1' = M.fromList [("hello",3),("bye",4)]
         m2' = M.fromList [("hello",5),("welcome",6)]
     in (m1' `M.union` m2', M.intersectionWith (-) m1' m2')

s = let set1 = S.insert "welcome" $ S.singleton "hello"
        set2 = S.fromList ["hello","bye"]
    in ( set1 `S.intersection` set2
       , "welcome" `S.member` set1
       , S.map length set2 )

-- | Exercise 4-3

clients = [GovOrg 1 "A", GovOrg 2 "B", Company 3 "C" (Person "C" "C") "C"]

classifyClients1 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients1 = foldr (\c m -> case c of
                            gov@(GovOrg {..}) -> f GovOrgKind gov m
                            com@(Company {..}) -> f CompanyKind com m
                            ind@(Individual {..}) -> f IndividualKind ind m
                   ) M.empty
                   where f kind c' m = M.insertWith S.union kind (S.singleton c') m

data ClientKind = GovOrgKind | CompanyKind | IndividualKind
                        deriving (Eq, Ord, Show)


-- data Tree   a = Node { rootLabel :: a, subForest :: Forest a }
-- type Forest a = [Tree a]
type Predicate a = a -> Bool

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees)
  = let subtreesTraversed = concat $ map (preOrder f) subtrees
    in f v : subtreesTraversed

pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                     , Node 6 [] ]

t1 = preOrder show pictureTree
t2 = flatten pictureTree
t3 = levels pictureTree
t4 = fmap (*2) pictureTree

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph = 
 [("wood","wood",["walls"]), ("plastic","plastic",["walls","wheels"])
 ,("aluminum","aluminum",["wheels","door"]),("walls","walls",["done"])
 ,("wheels","wheels",["done"]),("door","door",["done"]),("done","done",[])]
                     
timeMachinePrecedence
  :: (Graph, Vertex -> (String,String,[String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

timeMachineTravel :: Graph
timeMachineTravel = buildG (103,2013)
  [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
  ,(2013,1408),(1408,1993),(1408,917),(1993,917),(907,103),(103,917)]

g1 = let (g,v,_) = timeMachinePrecedence
     in map (\x -> let (k,_,_) = v x in k) $ topSort g
g2 = path timeMachineTravel 1302 917
g3 = reachable timeMachineTravel 1302
g4 = filter (\(Node { subForest = s }) -> s /= []) $ scc timeMachineTravel
g5 = map flattenSCC $ stronglyConnComp timeMachineGraph

class Nameable n where
  name :: n -> String

initial :: Nameable n => n -> Char
initial n = head (name n)

data Person = Person { firstName :: String, lastName :: String }
            deriving (Show, Eq, Ord, Read)
data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)

instance Nameable (Client i) where
  name Individual { person = Person { firstName = f, lastName = n } }
         = f ++ " " ++ n
  name c = clientName c

data Complex = C Double Double deriving (Show, Eq)

instance Num Complex where
  (C a1 b1) + (C a2 b2) = C (a1 + a2) (b1 + b2)
  (C a1 b1) - (C a2 b2) = C (a1 - a2) (b1 - b2)
  (C a1 b1) * (C a2 b2) = C (a1*a2-b1*b2) (a1*b2+b1*a2)
  negate (C a b)        = C (negate a) (negate b)
  fromInteger n         = C (fromInteger n) 0
  abs (C a b)           = C (sqrt $ a*a+b*b) 0
  signum c@(C a b)      = let C n _ = abs c in C (a / n) (b / n)

data TravelGuide = TravelGuide { title :: String
                               , authors :: [String]
                               , price :: Double }
                 deriving (Show, Eq, Ord)

data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1
                 | Leaf1
                 deriving Show

treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 t (Node1 v l r) = case compare t v of
                              EQ -> Just v
                              LT -> treeFind1 t l
                              GT -> treeFind1 t r
treeFind1 _ Leaf1         = Nothing

treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
treeInsert1 t n@(Node1 v l r) = case compare t v of
                                  EQ -> n
                                  LT -> Node1 v (treeInsert1 t l) r
                                  GT -> Node1 v l (treeInsert1 t r)
treeInsert1 t Leaf1           = Node1 t Leaf1 Leaf1

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                   | Leaf2
                   deriving Show

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case compare t v of
                              EQ -> Just v
                              LT -> treeFind2 t l
                              GT -> treeFind2 t r
treeFind2 _ Leaf2         = Nothing

newtype TGByPrice = TGByPrice TravelGuide deriving Eq

instance Ord TGByPrice where
  (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) =
     p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                     | Leaf3
                     deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c)
            => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r)
  = case compare v v2 of
      EQ -> Node3 v2 c2 l r
      LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
      GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3

treeInsert4 :: (Ord v, Monoid c)
            => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 l r)
  = case compare v v2 of
      EQ -> Node3 v2 c2 l r
      LT -> let newLeft = treeInsert4 v c l
                newCache = c2 <> cached newLeft <> cached r
            in Node3 v2 newCache newLeft r
      GT -> let newRight = treeInsert4 v c r
                newCache = c2 <> cached l <> cached newRight
            in Node3 v2 newCache l newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty

newtype Min = Min Double deriving Show

instance Semigroup Min where
  Min x <> Min y = Min $ min x y

instance Monoid Min where
  mempty  = Min infinity where infinity = 1/0
  mappend = (<>)  -- use the definition from Semigroup
