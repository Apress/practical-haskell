{-# LANGUAGE BangPatterns #-}
module Chapter05 where

import Data.List
import Control.DeepSeq

data TimeMachine = TM { manufacturer :: String, year :: Integer }
                 deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y+1)
timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

tm1 = take 3 timelyIncMachines
tm2 = find (\(TM { year = y }) -> y > 2018) timelyIncMachines
tm3 = length timelyIncMachines
tm4 = find (\(TM { year = y }) -> y == 10) timelyIncMachines

allNumbers :: [Integer]
allNumbers = allNumbersFrom 1
allNumbersFrom :: Integer -> [Integer]
allNumbersFrom n = n : allNumbersFrom (n+1)
an1 = zip allNumbers "abcd"
an2 = zip [1 .. ] "abcd"

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

infinite2020Machines :: [TimeMachine]
infinite2020Machines = TM "Timely Inc." 2020 : infinite2020Machines
tm5 = take 3 $ repeat $ TM "Timely Inc." 2020
specialOffer :: [TimeMachine]
specialOffer = cycle [TM m 2005, TM m 1994, TM m 908]
               where m = "Timely Inc."
tm6 = take 4 specialOffer

fibonacci2 :: [Integer]
fibonacci2 = map fst $ iterate (\(n,n1) -> (n1,n+n1)) (0,1)

sumForce :: [Integer] -> Integer
sumForce xs = sumForce' xs 0
   where sumForce' []     z = z
         sumForce' (y:ys) z = let s = z + y in s `seq` sumForce' ys s

sumYears :: [TimeMachine] -> Integer
sumYears xs = sumYears' xs 0
  where sumYears' []            z = z
        sumYears' (TM _ !y :ys) z = let !s = z + y in sumYears' ys s

data ListL a = ListL !Integer [a]

data Person = Person String String deriving Show
data Client = GovOrg     {-# UNPACK #-} !Int String
            | Company    {-# UNPACK #-} !Int String Person String
            | Individual {-# UNPACK #-} !Int Person
            deriving Show

instance NFData Client where
  rnf (GovOrg i n)                 = i `deepseq` n `deepseq` ()
  rnf (Company i n (Person f l) r) = i `deepseq` n `deepseq` f `deepseq` l 
                                       `deepseq` r `deepseq` ()
  rnf (Individual i (Person f l))  = i `deepseq` f `deepseq` l `deepseq` () 

