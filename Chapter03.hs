{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
module Chapter03 where

import Data.List
import Data.Function (on)
import GHC.Exts

-- Parametric polymorphism

maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)
                       -- Eq and Ord will be introduced in Chapter 4

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show, Eq, Ord)

nttf = GovOrg 'n' "NTTF"

data Triple a b c = Triple a b c
data SamePair a = SamePair a a

-- Exercise

swapTriple (x,y,z) = (y,z,x)

duplicate x = (x,x)

nothing _ = Nothing

index []     = []
index [x]    = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
               in  (n+1,x):indexed

maybeA [] = 'a'

-- Functions as parameters

fnExample1 = map succ [1,2,3]

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)

equalTuples :: [(Integer,Integer)] -> [Bool]
equalTuples t = map (\(x,y) -> x == y) t

sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
                                 "Alejandro" -> "Hello, writer"
                                 _           -> "Welcome, " ++ name
                     ) names

sayHello2 :: [String] -> [String]
sayHello2 names = map (\case "Alejandro" -> "Hello, writer"
                             name        -> "Welcome, " ++ name
                      ) names

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> n*x

double1 list = map (\x -> x * 2) list
double2      = \list -> map (\x -> x * 2) list
double3      = map (\x -> x * 2)
double4      = map (*2)

duplicateOdds1 list = map (*2) $ filter odd list
duplicateOdds2 = map (*2) . filter odd

-- More on modules

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter
  = filter (\l -> head l == letter) . permutations

-- Smart constructors and views

data Range = Range Integer Integer deriving Show

range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"

data RangeObs = R Integer Integer deriving Show

r :: Range -> RangeObs
r (Range a b) = R a b

prettyRange :: Range -> String
prettyRange rng = case rng of
                    (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]"

pattern R2 :: Integer -> Integer -> Range
pattern R2 a b <- Range a b
  where R2 a b = range a b

-- Folds

data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax MinusInfinity x       = x
infMax x MinusInfinity       = x
infMax PlusInfinity  _       = PlusInfinity
infMax _  PlusInfinity       = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

-- Filters

bothFilters :: (a -> Bool) -> [a] -> ([a],[a])
bothFilters p list = (filter p list, filter (not . p) list)

skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False ; _ -> True })

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _               = False

checkAnalytics :: [Client a] -> (Bool, Bool)
checkAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual{person = p1}) (Individual{person = p2})
                                = compare (firstName p1) (firstName p2)
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2             = compare (clientName c1) (clientName c2)

listOfClients
  = [ Individual 2 (Person "H. G." "Wells")
    , GovOrg 3 "NTTF"  -- National Time Travel Foundation
    , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist"
    , Individual 5 (Person "Doctor" "")
    , Individual 6 (Person "Sarah" "Jane")
    ]

companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics = map (duty . head) .
                           sortBy (\x y -> compare (length y) (length x)) .
                           groupBy (\x y -> duty x == duty y) .
                           filter isCompany
                         where isCompany (Company {}) = True
                               isCompany _            = False

companyDutiesAnalytics2 :: [Client a] -> [String]
companyDutiesAnalytics2 = map (duty . head) .
                           sortBy (flip  (compare `on` length)) .
                           groupBy ((==) `on` duty) .
                           filter isCompany
                         where isCompany (Company {}) = True
                               isCompany _            = False

-- Lists containing tuples

enum :: Int -> Int -> [Int]
enum a b | a > b = []
enum a b         = a : enum (a+1) b

withPositions :: [a] -> [(Int,a)]
withPositions list = zip (enum 1 $ length list) list

withPositions2 :: [a] -> [(Int,a)]
withPositions2 list = zip [1 .. length list] list

-- List comprehensions

duplicateOdds3 :: [Integer] -> [Integer]
duplicateOdds3 list = map (*2) $ filter odd list

duplicateOdds4 list = [ 2 * x | x <- list, odd x ]

companyAnalytics :: [Client a] -> [(String, [(Person, String)])]
companyAnalytics clients = [ (the clientName, zip person duty)
                           | client@(Company { .. }) <- clients
                           , then sortWith by duty
                           , then group by clientName using groupWith
                           , then sortWith by length client
                           ]
