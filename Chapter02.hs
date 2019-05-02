{-# language ViewPatterns #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
module Chapter02 where

-- Basic types

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

lst1 +++ lst2 = if null lst1  {- check emptyness -}
                then lst2      -- base case
                else (head lst1) : (tail lst1 +++ lst2)

reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]

maxmin list = if null (tail list)
              then (head list, head list)
              else ( if (head list) > fst (maxmin (tail list))
                     then head list
                     else fst (maxmin (tail list))
                   , if (head list) < snd (maxmin (tail list))
                     then head list
                     else snd (maxmin (tail list))
                   )

maxmin2 list = let h = head list
               in if null (tail list)
                  then (h, h)
                  else ( if h > t_max then h else t_max
                       , if h < t_min then h else t_min )
                       where t = maxmin2 (tail list)
                             t_max = fst t
                             t_min = snd t

-- Data types

data Client0 = GovOrg0     String
             | Company0    String Integer String String
             | Individual0 String String Bool
             deriving Show

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show
data Person = Person String String Gender
            deriving Show
data Gender = Male | Female
            deriving Show

clientName1 :: Client -> String
clientName1 client = case client of
                       GovOrg  name                -> name
                       Company name id person resp -> name
                       Individual person ads       ->
                           case person of
                             Person fNm lNm gender -> fNm ++ " " ++ lNm

clientName2 :: Client -> String
clientName2 client = case client of
                       GovOrg  name       -> name
                       Company name _ _ _ -> name
                       Individual (Person fNm lNm _) _ -> fNm ++ " " ++ lNm

companyName1 :: Client -> String
companyName1 client = case client of
                        Company name _ _ _ -> name

companyName2 :: Client -> Maybe String
companyName2 client = case client of
                        Company name _ _ _ -> Just name
                        _                  -> Nothing
f :: Client -> String
f client = case client of
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
             _                                  -> "There is no boss"

g :: Client -> String
g client = case client of
             Company _ _ (Person name _ _) pos -> 
               case pos of "Boss" -> name ++ " is the boss"
             _                               -> "There is no boss"

clientName3 (GovOrg name)                     = name
clientName3 (Company name _ _ _)              = name
clientName3 (Individual (Person fNm lNm _) _) = fNm ++ " " ++ lNm

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

(++++) :: [a] -> [a] -> [a]
list1 ++++ list2 = case list1 of
                     []   -> list2
                     x:xs -> x:(xs ++++ list2)

(+++++) :: [a] -> [a] -> [a]
[]     +++++ list2 = list2
(x:xs) +++++ list2 = x:(xs +++++ list2)

sorted :: [Integer] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:zs) = x < y && sorted (y:zs)

maxmin3 [x]    = (x,x)
maxmin3 (x:xs) = ( if x > xs_max then x else xs_max
                 , if x < xs_min then x else xs_min
                 ) where (xs_max, xs_min) = maxmin3 xs

-- Guards

ifibonacci :: Integer -> Maybe Integer
ifibonacci n = if n < 0
               then Nothing
               else case n of
                     0  -> Just 0
                     1  -> Just 1
                     n' -> let Just f1 = ifibonacci (n'-1)
                               Just f2 = ifibonacci (n'-2)
                           in Just (f1 + f2)

ifibonacci2 :: Integer -> Maybe Integer
ifibonacci2 n | n < 0     = Nothing
ifibonacci2 0             = Just 0
ifibonacci2 1             = Just 1
ifibonacci2 n | otherwise = let Just f1 = ifibonacci2 (n-1)
                                Just f2 = ifibonacci2 (n-2)
                            in Just (f1 + f2)
binom _ 0          = 1
binom x y | x == y = 1
binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

specialMultiples :: Integer -> String
specialMultiples n | multipleOf n 2 = show n ++ " is multiple of 2"
specialMultiples n | multipleOf n 3 = show n ++ " is multiple of 3"
specialMultiples n | multipleOf n 5 = show n ++ " is multiple of 5"
specialMultiples n | otherwise      = show n ++ " is a beautiful number"

specialMultiples2 :: Integer -> String
specialMultiples2 n 
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise      = show n ++ " is a beautiful number"

-- View patterns

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName1 -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _                               = False

-- Records

data ClientR = GovOrgR  { clientRName :: String }
             | CompanyR { clientRName :: String
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show

greet1, greet2, greet3 :: ClientR -> String

greet1 IndividualR { person = PersonR { firstName = fn } } = "Hi, " ++ fn
greet1 CompanyR    { clientRName = c } = "Hi, " ++ c
greet1 GovOrgR     { }                 = "Welcome"

greet2 IndividualR { person = PersonR { firstName } } = "Hi, " ++ firstName
greet2 CompanyR    { clientRName } = "Hi, " ++ clientRName
greet2 GovOrgR     { }             = "Welcome"

greet3 IndividualR { person = PersonR { .. } } = "Hi, " ++ firstName
greet3 CompanyR    { .. }                      = "Hi, " ++ clientRName
greet3 GovOrgR     { }                         = "Welcome"

-- Default values

data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data TimeOut = NoTimeOut | TimeOut Integer
data Connection = Connection

connect :: String -> ConnType -> Integer -> UseProxy
        -> Bool -> Bool -> TimeOut -> Connection
connect = undefined

connectUrl :: String -> Connection
connectUrl u = connect u TCP 0 NoProxy False False NoTimeOut

data ConnOptions = ConnOptions { connType      :: ConnType
                               , connSpeed     :: Integer
                               , connProxy     :: UseProxy
                               , connCaching   :: Bool
                               , connKeepAlive :: Bool
                               , connTimeOut   :: TimeOut
                               }

connect' :: String -> ConnOptions -> Connection
connect' url options = undefined

connDefault :: ConnOptions
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut

